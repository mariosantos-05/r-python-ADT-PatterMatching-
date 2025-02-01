use std::collections::HashMap;

use crate::ir::ast::{Expression, Name, Type, ValueConstructor};
type ErrorMessage = String;

type Environment = HashMap<Name, Type>;

pub fn check(exp: Expression, env: &Environment,) -> Result<Type, ErrorMessage> {
    match exp {
        Expression::CTrue => Ok(Type::TBool),
        Expression::CFalse => Ok(Type::TBool),
        Expression::CInt(_) => Ok(Type::TInteger),
        Expression::CReal(_) => Ok(Type::TReal),
        Expression::CString(_) => Ok(Type::TString),
        Expression::Add(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Sub(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Mul(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::Div(l, r) => check_bin_arithmetic_expression(*l, *r, env),
        Expression::And(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Or(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::Not(e) => check_not_expression(*e, env),
        Expression::EQ(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::LT(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::GTE(l, r) => check_bin_relational_expression(*l, *r, env),
        Expression::LTE(l, r) => check_bin_boolean_expression(*l, *r, env),
        Expression::ADTConstructor(name, exprs) => check_adt_constructor(name, exprs, env),
        _ => Err(String::from("not implemented yet")),
    }
}

fn check_adt_constructor(name: Name, exprs: Vec<Box<Expression>>, env: &Environment) -> Result<Type, ErrorMessage> {
    if let Some(Type::Tadt(_, value_constructors)) = env.get(&name) {
        for constructor in value_constructors {
            if constructor.types.len() == exprs.len() {
                for (i, expr) in exprs.iter().enumerate() {
                    let expr_type = check(*expr.clone(), env)?;
                    if expr_type != constructor.types[i] {
                        return Err(format!("Type mismatch in ADT constructor: expected {:?}, found {:?}", constructor.types[i], expr_type));
                    }
                }
                return Ok(Type::Tadt(name.clone(), value_constructors.clone()));
            }
        }
        Err(format!("Invalid ADT constructor usage for {}", name))
    } else {
        Err(format!("Unknown ADT: {}", name))
    }
}


fn check_bin_arithmetic_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TInteger),
        (Type::TInteger, Type::TReal) => Ok(Type::TReal),
        (Type::TReal, Type::TInteger) => Ok(Type::TReal),
        (Type::TReal, Type::TReal) => Ok(Type::TReal),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}

fn check_bin_boolean_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

    match (left_type, right_type) {
        (Type::TBool, Type::TBool) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting boolean type values.")),
    }
}

fn check_not_expression(exp: Expression, env: &Environment) -> Result<Type, ErrorMessage> {
    let exp_type = check(exp, env)?;

    match exp_type {
        Type::TBool => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting a boolean type value.")),
    }
}

fn check_bin_relational_expression(
    left: Expression,
    right: Expression,
    env: &Environment,
) -> Result<Type, ErrorMessage> {
    let left_type = check(left, env)?;
    let right_type = check(right, env)?;

    match (left_type, right_type) {
        (Type::TInteger, Type::TInteger) => Ok(Type::TBool),
        (Type::TInteger, Type::TReal) => Ok(Type::TBool),
        (Type::TReal, Type::TInteger) => Ok(Type::TBool),
        (Type::TReal, Type::TReal) => Ok(Type::TBool),
        _ => Err(String::from("[Type Error] expecting numeric type values.")),
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    use crate::ir::ast::Expression::*;
    use crate::ir::ast::Type::*;

    #[test]
    fn check_tlist_comparison() {
        let t_list1 = TList(Box::new(TInteger));
        let t_list2 = TList(Box::new(TInteger));

        assert_eq!(t_list1 == t_list2, true);
    }

    #[test]
    fn check_tlist_comparison_different_types() {
        let t_list1 = TList(Box::new(TInteger));
        let t_list2 = TList(Box::new(TBool));

        assert_eq!(t_list1 == t_list2, false);
    }

    #[test]
    fn check_ttuple_comparison() {
        let t_tuple1 = TTuple(vec![TInteger, TBool]);
        let t_tuple2 = TTuple(vec![TInteger, TBool]);

        assert_eq!(t_tuple1 == t_tuple2, true);
    }

    #[test]
    fn check_ttuple_comparison_different_types() {
        let t_tuple1 = TTuple(vec![TInteger, TBool]);
        let t_tuple2 = TTuple(vec![TBool, TInteger]);

        assert_eq!(t_tuple1 == t_tuple2, false);
    }

    #[test]
    fn check_constant() {
        let env = HashMap::new();
        let c10 = CInt(10);
        assert_eq!(check(c10, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_integers() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TInteger));
    }

    #[test]
    fn check_add_reals() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_real_and_integer() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let c20 = CReal(20.3);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TReal));
    }

    #[test]
    fn check_add_integer_and_real() {
        let env = HashMap::new();
        let c10 = CReal(10.5);
        let c20 = CInt(20);
        let add = Add(Box::new(c10), Box::new(c20));

        assert_eq!(check(add, &env), Ok(TReal));
    }

    #[test]
    fn check_type_error_arithmetic_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CFalse;
        let add = Add(Box::new(c10), Box::new(bool));

        assert_eq!(
            check(add, &env),
            Err(String::from("[Type Error] expecting numeric type values."))
        );
    }

    #[test]
    fn check_type_error_not_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let not = Not(Box::new(c10));

        assert_eq!(
            check(not, &env),
            Err(String::from("[Type Error] expecting a boolean type value."))
        );
    }

    #[test]
    fn check_type_error_and_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CTrue;
        let and = And(Box::new(c10), Box::new(bool));

        assert_eq!(
            check(and, &env),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }

    #[test]
    fn check_type_error_or_expression() {
        let env = HashMap::new();
        let c10 = CInt(10);
        let bool = CTrue;
        let or = Or(Box::new(c10), Box::new(bool));

        assert_eq!(
            check(or, &env),
            Err(String::from("[Type Error] expecting boolean type values."))
        );
    }


    fn setup_environment() -> Environment {
    let mut env = Environment::new();
    
    // Define the ADT "Some" with a constructor that takes a single integer
    let some_adt = vec![ValueConstructor {
        name: "Some".to_string(),
        types: vec![Type::TInteger], // Expected type for the constructor
    }];
    env.insert("Some".to_string(), Type::Tadt("Some".to_string(), some_adt));

    // Define the ADT "Maybe" with a constructor that takes a real number and a string
    let maybe_adt = vec![ValueConstructor {
        name: "Maybe".to_string(),
        types: vec![Type::TReal, Type::TString], // Expected types for the constructor
    }];
    env.insert("Maybe".to_string(), Type::Tadt("Maybe".to_string(), maybe_adt));

    env
}
    
    
    #[test]
    fn test_valid_adt_constructor() {
        let env = setup_environment();
        let exprs = vec![Box::new(Expression::CInt(10))]; // Argument type is TInteger
        let adt_name = "Some".to_string();
        let result = check_adt_constructor(adt_name, exprs, &env);
        
        assert_eq!(result, Ok(Type::Tadt("Some".to_string(), vec![
            ValueConstructor { name: "Some".to_string(), types: vec![Type::TInteger] }
        ])));
    }
    
    
    #[test]
    fn test_invalid_adt_constructor_argument_count() {
        let env = setup_environment();
        let exprs = vec![Box::new(Expression::CInt(10)), Box::new(Expression::CString("Hello".to_string()))]; // Extra argument
        let adt_name = "Some".to_string();
        let result = check_adt_constructor(adt_name, exprs, &env);
    
        assert_eq!(result, Err("Invalid ADT constructor usage for Some".to_string()));
    }
    
    
    #[test]
    fn test_invalid_adt_constructor_type_mismatch() {
        let env = setup_environment();
        let exprs = vec![Box::new(Expression::CString("Hello".to_string()))]; // Argument type should be TInteger
        let adt_name = "Some".to_string();
        let result = check_adt_constructor(adt_name, exprs, &env);
    
        assert_eq!(result, Err("Type mismatch in ADT constructor: expected TInteger, found TString".to_string()));
    }
    
    
    #[test]
    fn test_valid_adt_constructor_with_arguments() {
        let env = setup_environment();
        let exprs: Vec<Box<Expression>> = vec![
            Box::new(Expression::CReal(3.14)), // First argument: TReal
            Box::new(Expression::CString("Hello".to_string())), // Second argument: TString
        ];
        let adt_name = "Maybe".to_string();
        let result = check_adt_constructor(adt_name, exprs, &env);

        assert_eq!(result, Ok(Type::Tadt("Maybe".to_string(), vec![
            ValueConstructor { name: "Maybe".to_string(), types: vec![Type::TReal, Type::TString] }
        ])));
    }
}