use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Attribute, DataEnum,
    DataStruct, DataUnion, DeriveInput, Error, Field, GenericArgument, Ident, Meta, MetaList, Path,
    PathArguments, PathSegment, Result, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let generated = derive_impl(ast);

    match generated {
        Ok(token_stream) => token_stream,
        Err(error) => error.into_compile_error(),
    }
    .into()
}

fn derive_impl(ast: DeriveInput) -> Result<proc_macro2::TokenStream> {
    let struct_ident = &ast.ident;
    let builder_ident = format_ident!("{}Builder", struct_ident);

    let fields = get_fields(&ast)?;
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

    let required_functions = generate_required_functions(&required_fields);
    let optional_functions = generate_optional_functions(&optional_fields);
    let extend_functions = generate_extend_functions(&extend_fields)?;

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
                std::result::Result::Ok(#struct_ident {
                    #(#required_field_idents: self.#required_field_idents.clone().ok_or(concat!("Missing value for ", stringify!(#required_field_idents)))?,)*
                    #(#extend_field_idents: self.#extend_field_idents.clone().unwrap_or(Vec::new()),)*
                    #(#optional_field_idents: self.#optional_field_idents.clone().unwrap_or(None),)*
                })
            }
        }
    );

    Ok(generated)
}

fn group_fields(fields: &[Field]) -> (Vec<Field>, Vec<Field>, Vec<Field>) {
    let (optional_fields, required_fields): (Vec<_>, Vec<_>) = fields
        .iter()
        .cloned()
        .partition(|field| get_option_t(&field.ty).is_some());

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
                return segments.len() == 1 && segments.first().unwrap().ident == "builder";
            }
        }
        false
    });

    (required_fields, extend_fields, optional_fields)
}

fn generate_required_functions(fields: &[Field]) -> Vec<quote::__private::TokenStream> {
    fields
        .iter()
        .map(|field| {
            let ty = field.ty.clone();
            let ident = field.ident.clone();
            quote!(
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            )
        })
        .collect()
}

fn generate_optional_functions(fields: &[Field]) -> Vec<quote::__private::TokenStream> {
    fields
        .iter()
        .map(|field| {
            let ty =
                get_option_t(&field.ty).expect("Only fields with type Option<T> are supported");
            let ident = field
                .ident
                .clone()
                .expect("Only named structs are supported");

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
        let field_ident = field.ident.clone().expect("Only named Structs");
        let desired_ident = get_attribute_builder_each(field)?;
        let ty = &field.ty;
        let inner_ty = get_vec_t(ty).expect("Only fields with type Vec<T> are supported");

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

fn get_fields(ast: &DeriveInput) -> Result<Vec<Field>> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        Ok(named.clone().into_iter().collect())
    } else {
        let span = match ast.data {
            syn::Data::Struct(DataStruct { ref fields, .. }) => fields.span(),
            syn::Data::Union(DataUnion { union_token, .. }) => union_token.span(),
            syn::Data::Enum(DataEnum { enum_token, .. }) => enum_token.span(),
        };
        Err(Error::new(span, "expected named struct"))
    }
}

fn get_field_idents(fields: &[Field]) -> Vec<Ident> {
    fields
        .iter()
        .cloned()
        .map(|f| f.ident.expect("Only named structs are supported"))
        .collect()
}

/// This function returns the desired Ident for adding indivitual elements to a Vec<T> from the attribute `builder(each = "<Ident>")`
fn get_attribute_builder_each(field: &syn::Field) -> Result<Ident> {
    let error = || {
        Err(Error::new_spanned(
            field.clone(),
            "expected `builder(each = \"...\")`",
        ))
    };

    let attribute = if let Some(attribute) = field.attrs.iter().find(|attr| {
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
            false
        }
    }) {
        attribute
    } else {
        return error();
    };

    let desired_ident = if let Meta::List(ref list) = attribute.meta {
        list.tokens
            .clone()
            .into_iter()
            .find_map(|token| {
                if let proc_macro2::TokenTree::Ident(ident) = &token {
                    if ident != "each" {
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
            .unwrap_or(error())?
    } else {
        return error();
    };

    Ok(desired_ident)
}

fn get_option_t(ty: &syn::Type) -> Option<syn::Type> {
    // Check if it is an Option
    let segments = if let syn::Type::Path(TypePath {
        path: Path { ref segments, .. },
        ..
    }) = ty
    {
        segments
    } else {
        return None;
    };

    if segments.len() != 1 || segments.first().unwrap().ident != "Option" {
        return None;
    }

    // Check if it contains a generic T
    let segment = segments.first().unwrap();
    let args =
        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }) =
            segment.arguments
        {
            args
        } else {
            return None;
        };

    if args.len() != 1 {
        return None;
    }

    // Extract Type
    if let GenericArgument::Type(ty) = args.first().unwrap() {
        Some(ty.clone())
    } else {
        None
    }
}

fn get_vec_t(ty: &syn::Type) -> Option<syn::Type> {
    let type_path = if let Type::Path(ref type_path) = ty {
        type_path
    } else {
        return None;
    };

    let segment = type_path.path.segments.first()?;
    if segment.ident != "Vec" {
        return None;
    }

    let argument =
        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }, ..) =
            &segment.arguments
        {
            args.first()?
        } else {
            return None;
        };

    if let GenericArgument::Type(ty) = argument {
        return Some(ty.clone());
    }

    None
}
