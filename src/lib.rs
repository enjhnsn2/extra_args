#![feature(proc_macro_diagnostic)]
use proc_macro::TokenStream;
use quote::quote;
use syn::fold::Fold;
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Expr, ExprCall, ExprMethodCall, ExprPath, FnArg, Ident, ItemFn, Pat, Path,
    PathArguments, PathSegment, Signature, Type,
};

/// Right now macro should be used like:
/// #[with_extra_arg(trace: Trace)]

struct Args {
    decl: FnArg,
    var: Ident,
    ty: Type,
}

impl Args {
    fn decl(&self) -> FnArg {
        self.decl.clone()
    }

    fn var(&self) -> Ident {
        self.var.clone()
    }

    /*
    This is what the AST looks like for the inserted argument
    Path(
        ExprPath {
            attrs: [],
            qself: None,
            path: Path {
                leading_colon: None,
                segments: [
                    PathSegment {
                        ident: Ident {
                            ident: "state",
                            span: #0 bytes(744..745),
                        },
                        arguments: None,
                    },
                ],
            },
        },
    ),
    */

    /// Converts the state ident to an expression that can be used as an argument to functions
    fn var_as_expr(&self) -> Expr {
        let var = self.var();
        let segment = PathSegment {
            ident: var,
            arguments: PathArguments::None,
        };
        let path = Path::from(segment);
        let expr_path = ExprPath {
            attrs: Vec::new(),
            qself: None,
            path,
        };
        let expr = Expr::from(expr_path);
        expr
    }

    fn ty(&self) -> Type {
        self.ty.clone()
    }
}

fn pat_to_ident(pat: Pat) -> Result<Ident> {
    if let Pat::Ident(pat_ident) = pat {
        return Ok(pat_ident.ident);
    }

    Err(syn::Error::new(
        pat.span(),
        "Argument needs to be bound to an identifier",
    ))
}

// Parse arguments for macro
impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self> {
        let decl = syn::FnArg::parse(input)?;
        if let FnArg::Typed(ref pat_ty) = decl {
            let var = pat_to_ident(*pat_ty.pat.clone())?;
            let ty = *pat_ty.ty.clone();
            return Ok(Args { decl, var, ty });
        }

        Err(syn::Error::new(
            decl.span(),
            "You can't use self as an extra arg.",
        ))
    }
}

impl Fold for Args {
    /// Rewrite function signature of the form ... func(arg:Type,...) =>  func(arg:Type,..., state: &mut GlobalState)
    fn fold_signature(&mut self, sig: Signature) -> Signature {
        let arg_decl = self.decl(); // FnArg
        let mut new_sig = sig;
        new_sig.inputs.push(arg_decl);
        new_sig
    }

    /// Rewrite calls of the form func(...) => func(..., state)
    fn fold_expr_call(&mut self, call: ExprCall) -> ExprCall {
        let arg = self.var_as_expr();
        let mut new_call = call;
        new_call.args.push(arg);
        new_call
    }

    /// Rewrite method calls of the form o.method(...) => o.method(..., state)
    fn fold_expr_method_call(&mut self, method_call: ExprMethodCall) -> ExprMethodCall {
        let arg = self.var_as_expr();
        let mut new_method_call = method_call;
        new_method_call.args.push(arg);
        new_method_call
    }
}

#[cfg(feature = "enable")]
#[proc_macro_attribute]
pub fn with_extra_arg(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    // Parse the name binding and type of the global state
    let mut args = parse_macro_input!(args as Args);

    // Use a syntax tree traversal to transform the function body and signature.
    let output = args.fold_item_fn(input);

    // Hand the resulting function body back to the compiler.
    TokenStream::from(quote!(#output))
}

// if extra_arg is disabled, make a noop
#[cfg(not(feature = "enable"))]
#[proc_macro_attribute]
pub fn with_extra_arg(args: TokenStream, input: TokenStream) -> TokenStream {
    input
}

// #[proc_macro_attribute]
// pub fn all_with_extra_arg(args: TokenStream, input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as ItemFn);
//     // Parse the name binding and type of the global state
//     let mut args = parse_macro_input!(args as Args);

//     // Use a syntax tree traversal to transform the function body and signature.
//     println!("all_with_extra_args: input = {:#?}", input);
//     unimplemented!()
//     // let output = args.fold_item_fn(input);

//     // // Hand the resulting function body back to the compiler.
//     // TokenStream::from(quote!(#output))
// }
