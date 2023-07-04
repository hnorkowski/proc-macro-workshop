use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    eprintln!("{:#?}", ast);

    let ident = ast.ident.clone();
    let builder_ident = format_ident!("{}Builder", ident);

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = ast.data
    {
        named.into_iter()
    } else {
        unimplemented!()
    };

    let builder_fields = fields.clone().map(|mut field| {
        let ty = field.ty;
        let new_field = quote!(
            std::option::Option<#ty>
        );

        let optionized = syn::parse2::<syn::Type>(new_field).unwrap();
        field.ty = optionized;
        field
    });

    let field_idents = fields.clone().map(|f| f.ident.unwrap());
    let field_idents2 = field_idents.clone();
    let field_idents3 = field_idents.clone();

    let functions = fields.map(|field| {
        let ty = field.ty;
        let ident = field.ident;
        quote!(
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        )
    });

    let generated = quote!(
        pub struct #builder_ident {
            #(#builder_fields),*
        }

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_idents :None),*
                }
            }
        }

        impl #builder_ident {
            #(#functions)*

            pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                #(if self.#field_idents2.is_none(){
                    return std::result::Result::Err(format!("Missing value for {}", stringify!(#field_idents2)).into());
                })*

                std::result::Result::Ok(#ident {
                    #(#field_idents3: self.#field_idents3.clone().unwrap()),*
                })
            }
        }
    );

    generated.into()
}
