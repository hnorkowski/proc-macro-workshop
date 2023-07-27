use proc_macro::TokenStream;
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use syn::{parse_macro_input, Item, ItemEnum, ItemFn, PatTupleStruct, Stmt, __private::ToTokens};

#[proc_macro_attribute]
#[proc_macro_error]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let input2 = input.clone();
    let ast = parse_macro_input!(input2 as syn::Item);

    let _ = args;

    if let Err(err) = is_sorted(ast) {
        return err.into_compile_error().into();
    }

    input
}

fn is_sorted(ast: Item) -> syn::Result<()> {
    let enum2: ItemEnum = if let Item::Enum(enum2) = ast {
        enum2
    } else {
        abort_call_site!("expected enum or match expression");
    };

    let variants = enum2.variants;

    if variants.len() < 2 {
        return Ok(());
    }

    let prev_variant = variants.first().unwrap();
    for variant in variants.iter().skip(1) {
        if prev_variant.ident > variant.ident {
            abort!(
                variant.ident,
                "{} should sort before {}",
                variant.ident,
                prev_variant.ident
            );
        }
    }

    Ok(())
}

#[proc_macro_attribute]
#[proc_macro_error]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;

    let ast = parse_macro_input!(input as ItemFn);

    check_impl(ast).into()
}

fn check_impl(mut ast: ItemFn) -> proc_macro2::TokenStream {
    let statements = &mut ast.block.stmts;
    let mut error = None;

    for statement in statements {
        let match2 = if let Stmt::Expr(syn::Expr::Match(match2), ..) = statement {
            match2
        } else {
            continue;
        };

        let index = match2.attrs.iter().position(|e| {
            if let syn::Meta::Path(syn::Path { segments, .. }) = &e.meta {
                segments.len() == 1 && segments.first().unwrap().ident == "sorted"
            } else {
                false
            }
        });

        if let Some(index) = index {
            match2.attrs.remove(index);
        } else {
            continue;
        }

        // If an error occoured we can stop parsing but we still need to remove all #[sorted] attributes
        if error.is_some() {
            continue;
        }

        if match2.arms.len() < 2 {
            continue;
        }

        let mut iter = match2.arms.iter();
        let first = iter.next().unwrap();
        let mut last_pat = if is_supported_pat(first) {
            &first.pat
        } else {
            error = Some(syn::Error::new_spanned(
                &first.pat,
                "unsupported by #[sorted]",
            ));
            continue;
        };

        for arm in iter {
            let current_pat = if is_supported_pat(arm) {
                &arm.pat
            } else {
                error = Some(syn::Error::new_spanned(
                    &arm.pat,
                    "Not supported by #[sorted]",
                ));
                continue;
            };

            if current_pat.lt(last_pat) {
                error = Some(syn::Error::new_spanned(
                    current_pat.get_full_ident(),
                    format!(
                        "{} should sort before {}",
                        current_pat.to_string(),
                        last_pat.to_string(),
                    ),
                ));
                // We still need to continue parsing so we can return the original impl without the attributes
                // otherwise more errors will be thrown because of the missing fn
            }

            last_pat = current_pat;
        }
    }

    let mut stream = ast.into_token_stream();

    if let Some(error) = error {
        stream.extend(error.into_compile_error())
    }

    stream
}

fn is_supported_pat(arm: &syn::Arm) -> bool {
    matches!(
        &arm.pat,
        syn::Pat::Path(_)
            | syn::Pat::Ident(_)
            | syn::Pat::TupleStruct(_)
            | syn::Pat::Wild(syn::PatWild { .. })
    )
}

trait Ord {
    fn lt(&self, other: &Self) -> bool;
}

impl Ord for syn::Pat {
    fn lt(&self, other: &Self) -> bool {
        let self_ident = self.to_string();
        let other_ident = other.to_string();

        if self_ident == "_" {
            false
        } else if other_ident == "_" {
            true
        } else {
            self_ident < other_ident
        }
    }
}

trait ToString {
    fn to_string(&self) -> String;
}

impl ToString for syn::Pat {
    fn to_string(&self) -> String {
        self.get_full_ident().to_string().replace(' ', "")
    }
}

trait GetFullIdentTokenStream {
    fn get_full_ident(&self) -> proc_macro2::TokenStream;
}

impl GetFullIdentTokenStream for syn::Pat {
    fn get_full_ident(&self) -> proc_macro2::TokenStream {
        match self {
            syn::Pat::TupleStruct(PatTupleStruct { path, .. }) => path.to_token_stream(),
            other => other.to_token_stream(),
        }
    }
}
