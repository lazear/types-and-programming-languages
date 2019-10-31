#![allow(unused_variables, unused_macros)]
#[macro_use]
mod terms;
mod types;

use types::{Type, Type::*};
use util;

fn main() {
    let mut ctx = types::Context::default();

    let id = tyabs!(Type::Var(0), abs!(Type::Var(0), var!(0)));
    let id_bool = tyapp!(id.clone(), Nat);

    dbg!(ctx.type_of(&id_bool));
    dbg!(ctx.type_of(&app!(id_bool, lit!(true))));
}
