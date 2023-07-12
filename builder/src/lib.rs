use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Attribute, DeriveInput,
    Error, Field, GenericArgument, Ident, Meta, MetaList, Path, PathArguments, PathSegment, Result,
    Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let struct_ident = &ast.ident;
    let builder_ident = format_ident!("{}Builder", struct_ident);

    let fields = get_fields(&ast);

    let (required_fields, extend_fields, optional_fields) = group_fields(&fields);

    let builder_fields = fields.clone().into_iter().map(|mut field| {
        let ty = field.ty;
        let new_field = quote!(
            std::option::Option<#ty>
        );
        let attributes = Vec::new();

        let optionized = syn::parse2::<Type>(new_field).unwrap();
        field.ty = optionized;
        field.attrs = attributes;
        field
    });

    let field_idents = get_field_idents(&fields);

    let required_field_idents = get_field_idents(&required_fields);
    let extend_field_idents = get_field_idents(&extend_fields);
    let optional_field_idents = get_field_idents(&optional_fields);

    let required_functions = required_fields.iter().map(|field| {
        let ty = field.ty.clone();
        let ident = field.ident.clone();
        quote!(
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        )
    });

    let optional_functions = generate_optional_functions(&optional_fields);
    let extend_functions = generate_extend_functions(&extend_fields);

    let extend_functions = match extend_functions {
        Ok(functions) => functions,
        Err(error) => return error.into_compile_error().into(),
    };

    let generated = quote!(
        pub struct #builder_ident {
            #(#builder_fields,)*
        }

        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_idents: None,)*
                }
            }
        }

        impl #builder_ident {
            #(#required_functions)*
            #(#extend_functions)*
            #(#optional_functions)*

            pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
                #(if self.#required_field_idents.is_none(){
                    return std::result::Result::Err(format!("Missing value for {}", stringify!(#required_field_idents)).into());
                })*

                std::result::Result::Ok(#struct_ident {
                    #(#required_field_idents: self.#required_field_idents.clone().unwrap(),)*
                    #(#extend_field_idents: self.#extend_field_idents.clone().unwrap_or(Vec::new()),)*
                    #(#optional_field_idents: self.#optional_field_idents.clone().unwrap_or(None),)*
                })
            }
        }
    );

    generated.into()
}

fn get_fields(ast: &DeriveInput) -> Vec<Field> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named.clone().into_iter()
    } else {
        unimplemented!()
    }
    .collect()
}

fn group_fields(fields: &[Field]) -> (Vec<Field>, Vec<Field>, Vec<Field>) {
    let (optional_fields, required_fields): (Vec<_>, Vec<_>) =
        fields.iter().cloned().partition(|field| {
            if let syn::Type::Path(TypePath {
                path: Path { ref segments, .. },
                ..
            }) = field.ty
            {
                segments.len() == 1 && segments.first().unwrap().ident == "Option"
            } else {
                false
            }
        });

    let (extend_fields, required_fields) = required_fields.into_iter().partition(|field| {
        for attr in &field.attrs {
            if let Attribute {
                meta:
                    Meta::List(MetaList {
                        path: Path { ref segments, .. },
                        ..
                    }),
                ..
            } = attr
            {
                for segment in segments {
                    let PathSegment { ident, .. } = segment;
                    if ident == "builder" {
                        return true;
                    }
                }
            }
        }
        false
    });

    (required_fields, extend_fields, optional_fields)
}

fn get_field_idents(fields: &[Field]) -> Vec<Ident> {
    fields
        .iter()
        .cloned()
        .map(|f| f.ident.expect("Only NamedStructs are supported"))
        .collect()
}

fn generate_optional_functions(fields: &[Field]) -> Vec<quote::__private::TokenStream> {
    fields
        .iter()
        .map(|field| {
            let ty = if let Type::Path(TypePath {
                path: Path { ref segments, .. },
                ..
            }) = field.ty
            {
                let arguments = segments
                    .first()
                    .expect("Option<T> contains exactly one PathSegment")
                    .arguments
                    .clone();
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args, ..
                }) = arguments
                {
                    if let GenericArgument::Type(ty) = args
                        .first()
                        .expect("Option<T> contains exactly one TypeParameter")
                    {
                        ty.clone()
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                }
            } else {
                unreachable!("ty will always be an Option<T>")
            };

            let ident = field.ident.clone().unwrap();

            quote!(
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(Some(#ident));
                    self
                }
            )
        })
        .collect()
}

fn generate_extend_functions(fields: &[Field]) -> Result<Vec<quote::__private::TokenStream>> {
    let mut functions = Vec::new();

    for field in fields {
        let attribute = field
            .attrs
            .iter()
            .find(|attr| {
                if let Attribute {
                    meta:
                        Meta::List(MetaList {
                            path: Path { ref segments, .. },
                            ..
                        }),
                    ..
                } = attr
                {
                    return segments.iter().any(|segment| {
                        let PathSegment { ident, .. } = segment;
                        ident == "builder"
                    });
                } else {
                    unreachable!("All fields must have the attribute")
                }
            })
            .expect("All fields must have the attribute");

        let field_ident = field.ident.clone().expect("Only named Structs");

        let desired_ident = if let Meta::List(ref list) = attribute.meta {
            list.tokens
                .clone()
                .into_iter()
                .find_map(|token| {
                    if let proc_macro2::TokenTree::Ident(ident) = &token {
                        if ident != "each" {
                            eprintln!("Attr: {:#?}", attribute);
                            return Some(Err(Error::new_spanned(
                                attribute.meta.clone(),
                                "expected `builder(each = \"...\")`",
                            )));
                        }
                    }
                    if let proc_macro2::TokenTree::Literal(literal) = token {
                        let ident = literal.to_string().trim_matches('\"').to_string();
                        Some(syn::Result::Ok(Ident::new(&ident, ident.span())))
                    } else {
                        None
                    }
                })
                .expect("The attribute must always have an Ident")?
        } else {
            unreachable!("The attribute must always have a meta list");
        };

        let ty = &field.ty;
        let inner_ty = if let Type::Path(ref type_path) = field.ty {
            type_path
                .path
                .segments
                .iter()
                .find_map(|segment| {
                    if segment.ident == "Vec" {
                        if let PathArguments::AngleBracketed(
                            AngleBracketedGenericArguments { args, .. },
                            ..,
                        ) = segment.arguments.clone()
                        {
                            args.into_iter().find_map(|arg| {
                                if let GenericArgument::Type(Type::Path(path)) = arg {
                                    Some(path)
                                } else {
                                    None
                                }
                            })
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .expect("Only for Vec - Vec always have an inner type")
        } else {
            unreachable!()
        };
        if field_ident != desired_ident {
            functions.push(quote!(
                pub fn #field_ident(&mut self, value: #ty) -> &mut Self {
                    self.#field_ident = Some(value);
                    self
                }
            ));
        }

        functions.push(quote!(
            pub fn #desired_ident(&mut self, value: #inner_ty) -> &mut Self {
                let list = self.#field_ident.get_or_insert(Vec::new());
                list.push(value);
                self
            }
        ));
    }
    Ok(functions)
}
