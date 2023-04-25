use std::{path::Path, collections::{HashMap, HashSet}};

use swc_common::{
    self,
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecma_ast::{Stmt, Decl, ModuleItem, TsNamespaceBody, TsModuleName, ModuleDecl, TsModuleDecl, TsInterfaceDecl, TsTypeElement, TsType, TsKeywordType, TsKeywordTypeKind, TsPropertySignature, Module, ClassMember, TsEntityName, TsTypeOperator, TsTypeOperatorOp, TsUnionOrIntersectionType, TsUnionType, TsFnParam, TsTypeLit, Pat, PropName, Expr, VarDecl, VarDeclarator, TsEnumMemberId, Lit, UnaryOp, ParamOrTsParamProp, TsParamProp, TsParamPropParam, TsTypeParamDecl, TsLit, TsFnOrConstructorType};
use swc_ecma_parser::{lexer::Lexer, Capturing, Parser, StringInput, Syntax, TsConfig};

fn main() {
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));

    // Real usage
    // let fm = cm
    //     .load_file(Path::new("test.ts"))
    //     .expect("failed to load test.ts");

    let path = Path::new("/Users/jkbo/proj/rscode/trscode/node_modules/@types/vscode/index.d.ts");
    let fm = cm.load_file(path).unwrap();
        

    let lexer = Lexer::new(
        Syntax::Typescript(TsConfig { dts: true, decorators: false, ..Default::default()}),
        Default::default(),
        StringInput::from(&*fm),
        None,
    );

    let capturing = Capturing::new(lexer);

    let mut parser = Parser::new_from(capturing);

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
    }

    let module = parser
        .parse_typescript_module()
        .map_err(|e| e.into_diagnostic(&handler).emit())
        .expect("Failed to parse module.");
    
    let input = StringInput::from(&*fm);
    
    let typedb = Api::new(input.as_str(), &module, [
        Box::new(|api, ty| {
            let TsType::TsTypeRef(reftype) = ty else {
                return None;
            };
            let TsEntityName::Ident(n) = &reftype.type_name else {
                return None;
            };
            if "Thenable" != &n.sym.to_string() {
                return None;
            }
            Some(Type::Future(Box::new(
                api.resolve_type(&reftype.type_params.as_ref().unwrap().params.get(0).unwrap())
            )))
        })
    ]);
    typedb.log_stats();
    
    // // input.as_str()[]
    // for item in module.body.iter() {
    //     // println!("{item:?}");
    //     let ModuleItem::Stmt(Stmt::Decl(Decl::TsModule(decl))) = &item else {
    //         continue;
    //     };
    //     debug_module(input.as_str(), decl.as_ref(), 0);
    //     // let id = match &decl.id {
    //     //     TsModuleName::Ident(ident) => ident.to_string(),
    //     //     TsModuleName::Str(string) => string.value.to_string(),
    //     // };
    //     // println!("{id}");
    //     // for namespace in decl.body.iter() {
    //     //     let TsNamespaceBody::TsModuleBlock(mb) = namespace else {
    //     //         continue;
    //     //     };
    //     //     for item in mb.body.iter() {

    //     //     }
    //     //     println!("{namespace:?}");
    //     //     break;
    //     // }
    //     // break;
    // }


    // println!("Tokens: {:?}", parser.input().take());
}

fn debug_module(source: &str, decl: &TsModuleDecl, ident: usize) {
    let space = "";
    let width = 2 * ident;
    let id = match &decl.id {
        TsModuleName::Ident(ident) => ident.sym.to_string(),
        TsModuleName::Str(string) => string.value.to_string(),
    };
    println!("{space:width$}ns:{id}");
    let mut ifaces = 2;
    for namespace in decl.body.iter() {
        let TsNamespaceBody::TsModuleBlock(mb) = namespace else {
            continue;
        };
        // println!("{space:width$}{namespace:?}");
        for item in mb.body.iter() {
            if let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) = item {
                if let Decl::TsModule(module) = &export.decl {
                    debug_module(source, module.as_ref(), ident + 1);
                } else if let Decl::TsInterface(interface) = &export.decl {
                    // let id = interface.id.sym.to_string();
                    // println!("{space:width$}iface:{id}");
                    // write_interface(source, &interface);
                    ifaces -= 1;
                    if ifaces == 0 {
                        return;
                    }
                } else if let Decl::Fn(func) = &export.decl {
                    let id = func.ident.sym.to_string();
                    let span = func.function.span;
                    
                    
                    println!("{space:width$}func:{id}");
                    let decl = &source[span.lo.0 as usize - 1..span.hi.0 as usize];
                    println!("{space:width$}  {decl}");
                    // func.function.span
                }
            }
            // debug_module(item, ident + 1);
        }
        // break;
    }
}

// fn write_interface(source: &str, iface: &TsInterfaceDecl) {
//     let mut internal = format!("#[wasm_bindgen]\nextern \"C\" {{\n");
//     let name = iface.id.sym.to_string();
//     let mut ptrait = format!("pub trait I{name}: JsVal {{\n");
//     let mut pimpl = format!("impl I{name} {{\n");
//     internal = internal + format!("  pub type {name};\n").as_str();
//     for item in iface.body.body.iter() {
//         match item {
//             TsTypeElement::TsPropertySignature(prop) => {
//                 // sig = format!("{sig}: ");
//                 let typ = prop.type_ann.as_ref().unwrap();
//                 let mut typ = resolve_type(typ.type_ann.as_ref(), prop.readonly);
//                 if prop.optional {
//                     typ = typ.to_optional();
//                 }
//                 let ident = prop.key.as_ident().unwrap().sym.to_string();
//                 let ityp = typ.internal();
//                 let ptyp = typ.public();
//                 internal = internal + format!("  #[wasm_bindgen(method, getter, js_name = {ident})]\n").as_str();
//                 internal = internal + format!("  fn {ident}(this: &{name}) -> {ityp};\n").as_str();

//                 ptrait = ptrait + format!("  fn {ident}(&self) -> {ptyp};\n").as_str();
//                 pimpl = pimpl + format!("  fn {ident}(&self) -> {ptyp} {{\n").as_str();
//                 pimpl = pimpl + format!("    {}\n", typ.impl_getter(format!("self.{ident}()").as_str())).as_str();
//                 pimpl = pimpl + format!("  }}\n").as_str();
//             },
//             i => panic!("Can't handle interface element {i:?}")
            
//         }
//     }
//     internal = internal + "}\n";
//     ptrait = ptrait + "}\n";
//     pimpl = pimpl + "}\n";
//     println!("{internal}");
//     println!("{ptrait}");
//     println!("{pimpl}");
// }


fn resolve_keyword_type(typ: &TsKeywordType) -> Type {
    match typ.kind {
        TsKeywordTypeKind::TsAnyKeyword => Type::Primitive("JsValue"),
        TsKeywordTypeKind::TsBigIntKeyword => Type::Primitive("isize"),
        TsKeywordTypeKind::TsBooleanKeyword => Type::Primitive("bool"),
        TsKeywordTypeKind::TsNullKeyword => Type::None,
        TsKeywordTypeKind::TsUndefinedKeyword => Type::None,
        TsKeywordTypeKind::TsObjectKeyword => Type::Primitive("JsValue"),
        TsKeywordTypeKind::TsStringKeyword => Type::Primitive("String"),
        TsKeywordTypeKind::TsNumberKeyword => Type::Primitive("f32"),
        TsKeywordTypeKind::TsUnknownKeyword => Type::Primitive("JsValue"),
        TsKeywordTypeKind::TsVoidKeyword => Type::None,
        t => panic!("Unsupported keyword type: {t:?}")
    }
}
pub struct Repr {
    internal: String,
    public: String,
}
impl Repr {
    pub fn new<I: Into<String>, P: Into<String>>(internal: I, public: P) -> Self {
        let internal = internal.into();
        let public = public.into();
        Self { internal, public }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    None,
    Primitive(&'static str),
    Array(Box<Type>),
    ReadOnly(Box<Type>),
    ObjectMap(Box<Type>, Box<Type>),
    Optional(Box<Type>),
    Reference(String, Vec<Type>),
    Path(Vec<String>),
    Union(Vec<Type>),
    Tuple(Vec<Type>),
    Object(Vec<(String, Type)>),
    Future(Box<Type>),
    Function(Box<Function>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "Void"),
            Self::Primitive(p) => write!(f, "{p}"),
            Self::ReadOnly(p) => write!(f, "ReadOnly<{p}>"),
            Self::Array(a) => write!(f, "Array<{}>", a.as_ref()),
            Self::ObjectMap(k, v) => write!(f, "ObjMap<{}, {}>", k.as_ref(), v.as_ref()),
            Self::Optional(t) => write!(f, "Option<{}>", t.as_ref()),
            Self::Reference(r, p) => {
                let params = if p.is_empty() { "".to_string() } else {
                    format!("<{}>", p.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(", "))
                };
                write!(f, "{r}{params}")
            }
            Self::Path(p) => write!(f, "{}", p.join("::")),
            Self::Union(u) => write!(f, "Union<({})>", u.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(", ")),
            Self::Tuple(t) => write!(f, "({})", t.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(", ")),
            Self::Future(t) => write!(f, "Future<{t}>"),
            Self::Function(func) => {
                let args = func.args.iter().map(|a| format!("{}: {}", a.name, a.typ)).collect::<Vec<_>>().join(", ");
                let rtype = &func.return_type;
                write!(f, "fn({args}) -> {rtype}")
            },
            Self::Object(fields) => {
                let repr = fields.iter().map(|(n, t)| format!("{n}<{t}>")).collect::<Vec<_>>().join(", ");
                write!(f, "Obj<({repr})>")
            }
        }
    }
}

impl Type {
    
    fn internal(&self) -> String {
        match self {
            Self::Primitive(r) => r.to_string(),
            Self::Array(_) => "js_sys::Array".into(),
            Self::Optional(r) => format!("Option<{}>", r.internal()),
            t => panic!("{t:?} has not internal representation"),
        }
    }
    fn public(&self) -> String {
        match self {
            Self::Primitive(r) => r.to_string(),
            Self::Array(r) => format!("Array<{}>", r.public()),
            Self::Optional(r) => if r.is_array() {
                r.public()
            } else {
                format!("Option<{}>", r.public())
            },
            t => panic!("{t:?} has not public representation"),
            
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Self::Array(_) => true,
            _ => false
        }
    }

    pub fn is_optional(&self) -> bool {
        match self {
            Self::Optional(_) => true,
            _ => false
        }
    }

    pub fn to_optional(self) -> Self {
        Type::Optional(Box::new(self))
    }

    pub fn impl_getter(&self, prop: &str) -> String {
        match self {
            Self::Primitive(_) => format!("{prop}"),
            Self::Array(_) => format!("{prop}.into()"),
            Self::Optional(r) if r.is_array() => format!(
                "if let Some(prop) = {prop} {{ {} }} else {{ {} }}",
                r.impl_getter("prop"),
                r.impl_getter("Array::new()")
            ),
            Self::Optional(r) => format!("{prop}"),
            r => panic!("{r:?} has no getter implementation")
        }
    }

    pub fn populate_references(&self, refs: &mut HashSet<String>) {
        match self {
            Self::Array(t) => { t.populate_references(refs); },
            Self::None => { },
            Self::ReadOnly(t) => { t.populate_references(refs); },
            Self::ObjectMap(k, v) => {
                k.populate_references(refs);
                v.populate_references(refs);
            },
            Self::Optional(t) => { t.populate_references(refs); },
            Self::Path(p) => { 
                // TODO: it could be better way to populate refs from path
                refs.insert(p[0].clone());
            },
            Self::Primitive(_) => { },
            Self::Reference(t, _) => { refs.insert(t.clone()); },
            Self::Tuple(t) => t.iter().for_each(|t| t.populate_references(refs)),
            Self::Union(t) => t.iter().for_each(|t| t.populate_references(refs)),
            Self::Object(f) => f.iter().for_each(|(_, t)| t.populate_references(refs)),
            Self::Future(t) => t.populate_references(refs),
            Self::Function(f) => {
                f.return_type.populate_references(refs);
                f.args.iter().for_each(|a| a.typ.populate_references(refs));
            }
        };
    }
    pub fn map(&self, pmap: &HashMap<String, Type>) -> Type {
        match self {
            Self::Array(t) => Self::Array(Box::new(t.map(pmap))),
            Self::None => Self::None,
            Self::ReadOnly(t) => Self::ReadOnly(Box::new(t.map(pmap))),
            Self::ObjectMap(k, v) => Self::ObjectMap(Box::new(k.map(pmap)), Box::new(v.map(pmap))),
            Self::Optional(t) => Self::Optional(Box::new(t.map(pmap))),
            // TODO: do something with fkn path
            Self::Path(p) => Self::Path(p.clone()),
            Self::Primitive(t) => Self::Primitive(t.clone()),
            Self::Tuple(t) => Self::Tuple(t.iter().map(|t| t.map(pmap)).collect()),
            Self::Union(t) => Self::Union(t.iter().map(|t| t.map(pmap)).collect()),
            Self::Object(t) => Self::Object(t.iter().map(|(n, t)| (n.clone(), t.map(pmap))).collect()),
            Self::Future(t) => t.map(pmap),
            Self::Function(f) => {
                Self::Function(Box::new(Function {
                    name: f.name.clone(),
                    return_type: f.return_type.map(pmap),
                    args: f.args.iter().map(|a| {
                        Argument {
                            name: a.name.clone(),
                            optional: a.optional,
                            varargs: a.varargs,
                            typ: a.typ.map(pmap)
                        }
                    })
                    .collect()
                }))
            },
            Self::Reference(name, params) => {
                if pmap.contains_key(name) && params.is_empty() {
                    pmap.get(name).unwrap().clone()
                } else {
                    Self::Reference(name.clone(), params.iter().map(|t| t.map(pmap)).collect())
                }
            }
        }
    }

    pub fn is_simple_reference(&self, name: &String) -> bool {
        match self {
            Self::Reference(n, p) if n == name && p.is_empty() => true,
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct Property {
    name: String,
    typ: Type,
    readonly: bool,
    optional: bool
}

#[derive(Debug)]
pub struct Constructor {
    args: Vec<Argument>
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Function {
    name: String,
    return_type: Type,
    args: Vec<Argument>
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Argument {
    name: String,
    typ: Type,
    optional: bool,
    varargs: bool,
}

impl Argument {
    pub fn from_pat(typedb: &Api, param: &Pat) -> Self {
        match param {
            Pat::Ident(ident) => {
                let name = ident.id.sym.to_string();
                let mut typ = typedb.resolve_type(&ident.type_ann.as_ref().unwrap().type_ann);
                let optional = ident.optional;
                Self { name, typ, optional, varargs: false }
            },
            Pat::Rest(rest) => {
                let Pat::Ident(ident)  = rest.arg.as_ref() else {
                    panic!("Only ident rest params supported, got: {param:#?}")
                };
                let name = ident.sym.to_string();
                let typ = typedb.resolve_type(&rest.type_ann.as_ref().unwrap().type_ann);
                Self { name, typ, optional: true, varargs: true }
            },
            _ => panic!("Don't know how to process fnparam {param:#?}")
        }
    }
    pub fn from_ts_param(api: &Api, param: &TsParamProp) -> Self {
        match &param.param {
            TsParamPropParam::Ident(ident) => {
                let name = ident.sym.to_string();
                let mut typ = api.resolve_type(&ident.type_ann.as_ref().unwrap().type_ann);
                let optional = ident.optional;
                Argument { name, typ, optional, varargs: false }
            },
            TsParamPropParam::Assign(assign) => {
                panic!("TsParamPropParam::Assign not yet supported: {assign:#?}");
            }

        }
    }
    pub fn from_ts_fn_param(typedb: &Api, param: &TsFnParam) -> Self {
        match param {
            TsFnParam::Ident(ident) => {
                let name = ident.sym.to_string();
                let mut typ = typedb.resolve_type(ident.type_ann.as_ref().unwrap().type_ann.as_ref());
                let optional = ident.optional;
                Self { name, typ, optional, varargs: false }
            },
            TsFnParam::Rest(rest) => {
                let Pat::Ident(ident)  = rest.arg.as_ref() else {
                    panic!("Only ident rest params supported, got: {param:#?}")
                };
                let name = ident.sym.to_string();
                let typ = typedb.resolve_type(&rest.type_ann.as_ref().unwrap().type_ann);
                Self { name, typ, optional: true, varargs: true }
            }
            _ => panic!("Don't know how to process fnparam {param:#?}")
        }
    }
}

#[derive(Debug)]
pub enum Member {
    Property(Property),
    Method(Function),
    Constructor(Constructor),
    Call(Function),
    Index(Type, Type),
    Iterator,
    Unknown,
}

impl Member {
    pub fn is_constructor(&self) -> bool {
        match self {
            Self::Constructor(_) => true,
            _ => false
        }
    }

    pub fn from_interface_member(typedb: &Api, member: &TsTypeElement) -> Member {
        match member {
            TsTypeElement::TsPropertySignature(prop) => {
                let name = prop.key.as_ident().unwrap().sym.to_string();
                let readonly = prop.readonly;
                let optional = prop.optional;
                let typ = typedb.resolve_type(prop.type_ann.as_ref().unwrap().type_ann.as_ref());
                Member::Property(Property { name, typ, readonly, optional })
            },
            TsTypeElement::TsMethodSignature(method) => {
                let name = method.key.as_ident().unwrap().sym.to_string();
                let args = method.params.iter().map(|p| Argument::from_ts_fn_param(typedb, p)).collect();
                let return_type = typedb.resolve_type(method.type_ann.as_ref().unwrap().type_ann.as_ref());
                Member::Method(Function { name, args, return_type })
            },
            TsTypeElement::TsCallSignatureDecl(sig) => {
                let name = "".into();
                let args = sig.params.iter().map(|p| Argument::from_ts_fn_param(typedb, p)).collect();
                let return_type = typedb.resolve_type(&sig.type_ann.as_ref().unwrap().type_ann);
                Member::Call(Function { name, args, return_type })
            },
            TsTypeElement::TsIndexSignature(sig) => {
                if sig.params.len() != 1 {
                    panic!("Unsupported index signature {sig:#?}");
                }
                let key = &sig.params[0];
                let TsFnParam::Ident(key) = key else {
                    panic!("Unsupported index signature {sig:#?}");
                };
                let key = typedb.resolve_type(&key.type_ann.as_ref().unwrap().type_ann);
                let value = typedb.resolve_type(&sig.type_ann.as_ref().unwrap().type_ann);
                Member::Index(key, value)
            }
            _ => panic!("Unsupported interface member {member:#?}")
        }
    }

    pub fn from_class_member(typedb: &Api, member: &ClassMember) -> Member {
        // panic!("wanna add class member {member:#?}")
        match member {
            ClassMember::ClassProp(prop) => {
                let name = prop.key.as_ident().unwrap().sym.to_string();
                let readonly = prop.readonly;
                let optional = prop.is_optional;
                let typ = typedb.resolve_type(prop.type_ann.as_ref().unwrap().type_ann.as_ref());
                Member::Property(Property { name, typ, readonly, optional })
            },
            ClassMember::Method(method) => {
                if let PropName::Computed(name) = &method.key {
                    if let Expr::Member(member) = name.expr.as_ref() {
                        if member.prop.is_ident() && member.prop.as_ident().unwrap().sym.to_string().as_str() == "iterator" {
                            // TODO: think about iterators
                            return Member::Iterator
                        }
                    }
                }
                // if let PropName::
                let name = method.key.as_ident().expect(format!("{member:#?}").as_str()).sym.to_string();
                let args = method.function.params.iter().map(|p| Argument::from_pat(typedb, &p.pat)).collect();
                let return_type = typedb.resolve_type(&method.function.return_type.as_ref().unwrap().type_ann);
                Member::Method(Function {name, args, return_type})
            },
            ClassMember::Constructor(constructor) => {
                let args = constructor.params.iter().map(|p| match p {
                    ParamOrTsParamProp::Param(p) => Argument::from_pat(typedb, &p.pat),
                    ParamOrTsParamProp::TsParamProp(p) => Argument::from_ts_param(typedb, p)
                }).collect();
                Member::Constructor(Constructor { args })
            }
            _ => panic!("Unsupported class member {member:#?}")
        }
    }
}


#[derive(Debug, PartialEq)]
pub enum StructKind {
    Interface,
    Class,
}
#[derive(Debug)]
pub struct Struct {
    name: String,
    members: Vec<Member>,
    params: Vec<(String, Type)>,
    kind: StructKind,
}

impl Struct {
    pub fn as_reference(&self) -> Type {
        Type::Reference(
            self.name.clone(),
            self.params.iter().map(|(n, _)| Type::Reference(n.clone(), vec![])).collect()
        )
    }
    pub fn constructable(&self) -> bool {
        match self.kind {
            StructKind::Interface => false,
            StructKind::Class => self.members
                .iter()
                .filter(|m| m.is_constructor())
                .count() > 0
        }
    }
}

#[derive(Debug)]
pub struct Alias {
    name: String,
    typ: Type,
}

#[derive(Debug)]
pub struct Var {
    name: String,
    typ: Type
}

#[derive(Debug)]
pub struct Enum {
    name: String,
    members: Vec<(String, i32)>
}

#[derive(Debug)]
pub enum ApiMember {
    Struct(Struct),
    Function(Function),
    Functions(Vec<Function>),
    Alias(Alias),
    Var(Var),
    Enum(Enum),
}

impl ApiMember {
    pub fn name(&self) -> &String {
        match self {
            Self::Struct(s) => &s.name,
            Self::Function(f) => &f.name,
            Self::Functions(f) => &f[0].name,
            Self::Alias(a) => &a.name,
            Self::Var(v) => &v.name,
            Self::Enum(e) => &e.name,
        }
    }
    pub fn add_output_type(&self, api: &Api, output: &mut HashSet<Type>, params: &Vec<Type>, mut pmap: HashMap<String, Type>) {
        match self {
            ApiMember::Enum(_) => { /* TODO: should I ignore enum content? */}
            ApiMember::Alias(t) => api.add_output_type(t.typ.map(&pmap), output, pmap),
            ApiMember::Function(f) => api.add_output_type(f.return_type.map(&pmap), output, pmap),
            ApiMember::Functions(f) => f.iter().for_each(|f| api.add_output_type(f.return_type.map(&pmap), output, pmap.clone())),
            ApiMember::Var(v) => api.add_output_type(v.typ.map(&pmap), output, pmap),
            ApiMember::Struct(s) => {
                let generic = Type::Reference(
                    s.name.clone(),
                    s.params.iter().map(|(n, _)| Type::Reference(n.clone(), vec![])).collect()
                );
                output.insert(generic);
                for (idx, (pname, _)) in s.params.iter().enumerate() {
                    if params[idx].is_simple_reference(pname) {
                        continue;
                    }
                    pmap.insert(pname.clone(), params[idx].clone());
                }
                for member in s.members.iter() {
                    match member {
                        Member::Property(p) => {
                            if let Some(ty) = pmap.resolve(&p.typ) {
                                api.add_output_type(ty, output, pmap.clone())
                            } else {
                                api.add_output_type(p.typ.map(&pmap), output, pmap.clone())
                            }
                        },
                        Member::Index(_, v) => {
                            if let Some(ty) = pmap.resolve(v) {
                                api.add_output_type(ty, output, pmap.clone());
                            } else {
                                api.add_output_type(v.map(&pmap), output, pmap.clone());
                            }
                        }
                        Member::Method(m) => {
                            if let Some(ty) = pmap.resolve(&m.return_type) {
                                api.add_output_type(ty, output, pmap.clone());
                            } else {
                                api.add_output_type(m.return_type.map(&pmap), output, pmap.clone());
                            }
                        }
                        Member::Call(c) => {
                            if let Some(ty) = pmap.resolve(&c.return_type) {
                                api.add_output_type(ty, output, pmap.clone());
                            } else {
                                api.add_output_type(c.return_type.map(&pmap), output, pmap.clone());
                            }
                        }
                        Member::Constructor(_) => { /* skip */},
                        Member::Iterator => { /* TODO: handle iterators */},
                        Member::Unknown => { /* TODO: get rid of Unknown */}
                    }
                }
            }
        }
    }
}

pub trait TypeMapImpl {
    fn resolve(&self, ty: &Type) -> Option<Type>;
}

impl TypeMapImpl for HashMap<String, Type> {
    fn resolve(&self, ty: &Type) -> Option<Type> {
        let arg = match ty {
            Type::Reference(name, params) if params.is_empty() => name,
            _ => return None
        };
        self.get(arg).cloned()
    }
}

pub type TypeResolver = Box<dyn Fn(&Api, &TsType) -> Option<Type>>;

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct ModuleName(Vec<String>);

impl ModuleName {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn to_js_name(&self) -> String {
        self.0.join(".")
    }
    pub fn to_rs_name(&self) -> String {
        self.0.join("::")
    }
}

impl std::ops::Deref for ModuleName {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ModuleName {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Display for ModuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_rs_name())
    }
}

pub struct Api { 
    members: HashMap<String, ApiMember>,
    parsing_module: ModuleName,
    module_map: HashMap<String, ModuleName>,
    resolvers: Vec<TypeResolver>,
}

impl std::ops::Deref for Api {
    type Target = HashMap<String, ApiMember>;
    fn deref(&self) -> &Self::Target {
        &self.members
    }
}
impl std::ops::DerefMut for Api {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.members
    }
}

impl Api {
    pub fn new<const R: usize>(source: &str, module: &Module, resolvers: [TypeResolver; R]) -> Self {
        let mut typedb = Api {
            members: HashMap::new(),
            parsing_module: ModuleName::new(),
            module_map: HashMap::new(),
            resolvers: resolvers.into(),
        };
        for item in module.body.iter() {
            let ModuleItem::Stmt(Stmt::Decl(Decl::TsModule(decl))) = &item else {
                continue;
            };
            typedb.add_module(decl)
        }
        typedb
    }

    pub fn insert(&mut self, key: String, value: ApiMember) {
        if self.contains_key(&key) {
            panic!("Member {key} already added");
        }
        let ns = &self.parsing_module;
        if let Some(declared_ns) = self.module_map.get(&key) {
            if declared_ns != ns {
                println!("Member {ns}::{key} already declared in module {declared_ns}");
                return
            }
        }
        self.module_map.insert(key.clone(), ns.clone());
        self.members.insert(key, value);
    }

    pub fn add_class(&mut self, class: Struct) {
        self.insert(class.name.clone(), ApiMember::Struct(class));
    }
    pub fn add_interface(&mut self, interface: Struct) {
        self.insert(interface.name.clone(), ApiMember::Struct(interface));
    }
    pub fn add_function(&mut self, func: Function) {
        let name = func.name.clone();
        match self.members.remove(&name) {
            Some(ApiMember::Functions(mut funcs)) => {
                funcs.push(func);
                self.insert(name, ApiMember::Functions(funcs));
            },
            Some(ApiMember::Function(f)) => {
                let mut funcs = vec![];
                funcs.push(f);
                funcs.push(func);
                self.insert(name, ApiMember::Functions(funcs));
            },
            Some(_) => {
                panic!("Function {name} attempts to overwrite existed member");
            }
            None => {
                self.insert(name, ApiMember::Function(func));
            }
        }
    }
    pub fn add_alias(&mut self, alias: Alias) {
        self.insert(alias.name.clone(), ApiMember::Alias(alias));
    }
    pub fn add_var(&mut self, var: Var) {
        self.insert(var.name.clone(), ApiMember::Var(var));
    }
    pub fn add_enum(&mut self, en: Enum) {
        self.insert(en.name.clone(), ApiMember::Enum(en));
    }

    pub fn add_module(&mut self, module: &TsModuleDecl) {
        let module_name = match &module.id {
            TsModuleName::Ident(ident) => ident.sym.to_string(),
            TsModuleName::Str(value) => value.value.to_string(),
        };
        println!("Adding module {module_name}");
        self.parsing_module.push(module_name.clone());
        self.add_namespace(module.body.as_ref().unwrap());
        self.parsing_module.pop();
    }

    fn add_namespace(&mut self, ns: &TsNamespaceBody) {
        match ns {
            TsNamespaceBody::TsModuleBlock(block) => {
                for item in block.body.iter() {
                    let ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export)) = item else {
                        continue
                    };
                    match &export.decl {
                        Decl::Class(class) => {
                            let class_name = class.ident.sym.to_string();
                            println!("Adding class {class_name}");
                            let mut cc = Struct {
                                name: class_name.clone(),
                                kind: StructKind::Class,
                                members: vec![],
                                params: class.class.type_params.into_type_params(self)
                            };
                            for member in class.class.body.iter() {
                                cc.members.push(Member::from_class_member(self, member));
                            }
                            self.add_class(cc);
                            
                        },
                        Decl::TsInterface(interface) => {
                            let interface_name = interface.id.sym.to_string();
                            println!("Adding interface {interface_name}");
                            let mut cc = Struct {
                                name: interface_name.clone(),
                                kind: StructKind::Class,
                                members: vec![],
                                params: interface.type_params.into_type_params(self)
                            };
                            for member in interface.body.body.iter() {
                                cc.members.push(Member::from_interface_member(self, member))
                            }
                            self.add_interface(cc);
                        },
                        Decl::Fn(func) => {
                            let name = func.ident.sym.to_string();
                            let args = func.function.params.iter().map(|p| Argument::from_pat(self, &p.pat)).collect();
                            let return_type = self.resolve_type(&func.function.return_type.as_ref().unwrap().type_ann);
                            let func = Function { name, args, return_type };
                            println!("Adding function {}", func.name);
                            self.add_function(func);
                        },
                        Decl::TsEnum(tsenum) => {
                            // panic!("enum {tsenum:#?}");
                            let name = tsenum.id.sym.to_string();
                            let members = tsenum.members.iter().map(|e| {
                                let TsEnumMemberId::Ident(ident) = &e.id else {
                                    panic!("Unsupported enum declaration: {tsenum:#?}");
                                };
                                let value = match e.init.as_ref().unwrap().as_ref() {
                                    Expr::Unary(expr) => match &expr.op {
                                        UnaryOp::Minus => {
                                            let Expr::Lit(Lit::Num(expr)) = expr.arg.as_ref() else {
                                                panic!("Unsupported enum value: {tsenum:#?}");
                                            };
                                            -expr.value as i32
                                        },
                                        UnaryOp::Plus => {
                                            let Expr::Lit(Lit::Num(expr)) = expr.arg.as_ref() else {
                                                panic!("Unsupported enum value: {tsenum:#?}");
                                            };
                                            expr.value as i32
                                        },
                                        _ => panic!("Unsupported enum value: {tsenum:#?}")
                                    },
                                    Expr::Lit(Lit::Num(expr)) => expr.value as i32,
                                    _ => panic!("Unsupported enum value: {tsenum:#?}"),
                                };
                                (ident.sym.to_string(), value)
                            })
                            .collect();
                            self.add_enum(Enum { name, members })


                        },
                        Decl::TsTypeAlias(alias) => {
                            let name = alias.id.sym.to_string();
                            let typ = self.resolve_type(&alias.type_ann.as_ref());
                            self.add_alias(Alias { name, typ })

                        },
                        Decl::Var(var) => {
                            for decl in var.decls.iter() {
                                let Pat::Ident(ident) = &decl.name else {
                                    panic!("Unsupported var declaration: {decl:#?}")
                                };
                                let name = ident.id.sym.to_string();
                                let typ = self.resolve_type(&ident.type_ann.as_ref().unwrap().type_ann);
                                self.add_var(Var { name, typ }) 
                            }
                        },
                        Decl::TsModule(module) => {
                            self.add_module(module.as_ref())
                        }
                    }
                    
                }
            },
            TsNamespaceBody::TsNamespaceDecl(decl) => {
                panic!("Don't know how to add ns decl {decl:#?}")
            }

        }
        // for item in ns.body.body.iter() {
        //     match item {
        //         TsTypeElement::TsPropertySignature(prop) => { },
        //         TsTypeElement::TsConstructSignatureDecl(construct) => {

        //         }
    }

    fn resolve_type(&self, typ: &TsType) -> Type {
        if let Some(typ) = self.resolvers.iter().find_map(|r| r(self, typ)) {
            return typ
        };
        match typ {
            TsType::TsKeywordType(t) => resolve_keyword_type(t),
            TsType::TsTypeRef(t) => {
                let params = if let Some(params) = t.type_params.as_ref() {
                    params.params.iter().map(|t| self.resolve_type(t)).collect()
                } else {
                    vec![]
                };
                t.type_name.into_type(self, params)
            },
            TsType::TsTypeOperator(op) => match &op.op {
                TsTypeOperatorOp::ReadOnly => self.resolve_type(op.type_ann.as_ref()),
                op => panic!("Unsupported type operator {op:?}")
            },
            TsType::TsUnionOrIntersectionType(u) => match u {
                TsUnionOrIntersectionType::TsUnionType(u) => {
                    if let Some(t) = u.into_optional_type(self) {
                        t
                    } else {
                        let mut types = vec![];
                        for t in u.types.iter() {
                            types.push(self.resolve_type(t.as_ref()))
                        }
                        Type::Union(types)
                    }
                },
                u => {
                    // TODO: think about intersections
                    // panic!("Interection type not supported: {u:?}")
                    Type::None
                }
            }
            TsType::TsArrayType(t) => {
                let elem = self.resolve_type(t.elem_type.as_ref());
                Type::Array(Box::new(elem))
            },
            TsType::TsTypeLit(t) => {
                if t.is_object_map() {
                    t.resolve_object_map(self)
                } else {
                    t.resolve_anonymous_object(self)
                }
            },
            TsType::TsTupleType(t) => {
                Type::Tuple(t.elem_types
                    .iter()
                    .map(|t| self.resolve_type(&t.ty))
                    .collect()
                )
            },
            TsType::TsFnOrConstructorType(t) => {
                let TsFnOrConstructorType::TsFnType(f) = t else {
                    panic!("Expected TsFnOrConstructorType::TsFnType, got {t:#?}")
                };
                let args = f.params.iter().map(|p| Argument::from_ts_fn_param(self, p)).collect();
                let return_type = self.resolve_type(&f.type_ann.as_ref().type_ann);
                Type::Function(Box::new(Function {
                    name: format!(""),
                    args,
                    return_type
                }))
                
            },
            TsType::TsParenthesizedType(t) => {
                self.resolve_type(&t.type_ann)
            },
            TsType::TsLitType(lit) => {
                match &lit.lit {
                    TsLit::Bool(_) => Type::Primitive("bool"),
                    lit => panic!("Can't resolve lit type {lit:#?}")
                }
            }

            t => panic!("Can't resolve type '{t:#?}")
        }
    }

    pub fn log_stats(&self) {
        let mut types = HashSet::new();
        for (_, member) in self.members.iter() {
            match member {
                ApiMember::Alias(alias) => alias.typ.populate_references(&mut types),
                ApiMember::Enum(en) => { types.insert(en.name.clone()); },
                ApiMember::Function(func) => {
                    func.args.iter().for_each(|a| a.typ.populate_references(&mut types));
                    func.return_type.populate_references(&mut types);
                },
                ApiMember::Functions(func) => {
                    func.iter().for_each(|func| {
                        func.args.iter().for_each(|a| a.typ.populate_references(&mut types));
                        func.return_type.populate_references(&mut types);
                    });
                }
                ApiMember::Struct(s) => s.members.iter().for_each(|m| {
                    match m {
                        Member::Constructor(c) => c.args.iter().for_each(|a| a.typ.populate_references(&mut types)),
                        Member::Property(p) => p.typ.populate_references(&mut types),
                        Member::Method(func) => {
                            func.args.iter().for_each(|a| a.typ.populate_references(&mut types));
                            func.return_type.populate_references(&mut types);
                        },
                        _ => { }
                    }
                }),
                ApiMember::Var(v) => v.typ.populate_references(&mut types)
            };
        }
        println!("Refrenced types used: {}", types.len());
        let types_parsed = self.members
            .iter()
            .filter(|(_, m)| {
                match m {
                    ApiMember::Function(_) => false,
                    ApiMember::Var(_) => false,
                    _ => true
                }
            })
            .count();
        println!("Refrenced types parsed: {types_parsed}");

        let output = self.find_output_types();
        println!("");
        println!("Output types:");
        for t in output {
            println!("  {t}");
        }

        println!("");
        println!("Funcs:");
        for (key, member) in self.members.iter() {
            let module = self.module_map.get(key).unwrap();
            if &module.to_rs_name() != "vscode::commands" {
                continue
            }
            let ApiMember::Function(func) = member else {
                continue
            };
            let name = &func.name;
            let args = func.args.iter().map(|a| format!("{}: {}", a.name, a.typ)).collect::<Vec<_>>().join(", ");
            let rt = &func.return_type;
            println!("  fn {name}({args}) -> {rt};");
        }

    }

    pub fn find_output_types(&self) -> HashSet<Type> {
        let mut output = HashSet::new();
        self.add_output_type(Type::Reference("ExtensionContext".into(), vec![]), &mut output, HashMap::new());
        for (key, member) in self.members.iter() {
            let module = self.module_map.get(key).unwrap();
            if &module.to_js_name() != "vscode::commands" {
                continue
            }
            // if mod
            println!("{}::{key}", self.module_map.get(key).unwrap());
            match member {
                ApiMember::Function(f) => self.add_output_type(f.return_type.clone(), &mut output, HashMap::new()),
                ApiMember::Struct(s) if s.constructable() => self.add_output_type(s.as_reference(), &mut output, HashMap::new()),
                _ => { }
            }
        }
        output
    }

    pub fn add_output_type(&self, ty: Type, output: &mut HashSet<Type>, pmap: HashMap<String, Type>) {
        if output.contains(&ty) {
            return;
        }
        let this = ty.clone();
        match ty {
            Type::None => { /* ignore */},
            Type::Array(t) => self.add_output_type(t.map(&pmap), output, pmap),
            Type::ReadOnly(t) => self.add_output_type(t.map(&pmap), output, pmap),
            Type::Optional(t) => self.add_output_type(t.map(&pmap), output, pmap),
            Type::Path(_) => { /* TODO: add path somehow? */},
            Type::Primitive(_) => { /* ignore */},
            Type::Tuple(t) => { t.iter().for_each(|t| self.add_output_type(t.map(&pmap), output, pmap.clone()) )},
            Type::Union(t) => {
                output.insert(this.map(&pmap));
                t.iter().for_each(|t| self.add_output_type(t.map(&pmap), output, pmap.clone()));
            },
            Type::ObjectMap(k, v) => {
                self.add_output_type(k.map(&pmap), output, pmap.clone());
                self.add_output_type(v.map(&pmap), output, pmap);
            },
            Type::Reference(r, params) => {
                let Some(member) = self.members.get(&r) else {
                    println!("Skipping output reference type {r} from {this}");
                    return;
                };
                member.add_output_type(self, output, &params, pmap);
            },
            Type::Future(t) => self.add_output_type(t.map(&pmap), output, pmap),
            Type::Function(f) => self.add_output_type(f.return_type.map(&pmap), output, pmap),
            Type::Object(fields) => {
                output.insert(this.map(&pmap));
                fields.iter().for_each(|(_, t)| self.add_output_type(t.map(&pmap), output, pmap.clone()));
            }
        }
    }
}


pub struct ModuleWriter {
    module: ModuleName,
    public: String,
    internal: String
}

#[derive(Default)]
pub struct SourceWriter {
    source: String,
    ident: usize
}

impl SourceWriter {
    pub fn add(&mut self, s: impl AsRef<str>) {
        let s = s.as_ref();
        self.source += format!("{:width$}{s}\n", " ", width=self.ident*4).as_str();
    }
    pub fn ident(&mut self, f: impl FnOnce(&mut Self)) {
        self.ident += 1;
        f(self);
        self.ident -= 1;
    }
}

pub struct Writer {
    path: String,
    modules: HashMap<ModuleName, ModuleWriter>
}

impl Writer {
    pub fn write(&mut self, member: &ApiMember, api: &Api) {
        let name = member.name();
        if name.is_empty() {
            panic!("Can't generate code for nameles ApiMember {member:#?}")
        }
        let module_name = api.module_map.get(name).unwrap();
        let module = self.modules.entry(module_name.clone()).or_insert_with(|| {
            ModuleWriter {
                module: module_name.clone(),
                public: String::new(),
                internal: String::new()
            }
        });

        match member {
            ApiMember::Struct(s) if s.kind == StructKind::Interface => {
                self.write_interface(s, api)
            },
            ApiMember::Struct(s) if s.kind == StructKind::Class => {
                self.write_class(s, api)
            },
            _ => { }
        }
    }

    fn write_interface(&mut self, interface: &Struct, api: &Api) {
        // let mut public = SourceWriter::default();
        // let mut internal = SourceWriter::default();
        // internal.add(format!("#["))


    }

    fn write_class(&mut self, class: &Struct, api: &Api) {
        let mut public = SourceWriter::default();
        let mut internal = SourceWriter::default();
        let module_name = api.module_map.get(&class.name).unwrap();

        internal.add(format!("#[wasm_bindgen(module = \"{}\")]", module_name.to_js_name()));
        internal.add("extern \"C\"{");
        internal.ident(|internal| {
            internal.add(format!("pub type {};", class.name));
            for member in class.members.iter() {
                // match 
            }
        });
        internal.add("}");


    }
}

pub trait StringExt {
    fn capitalize(&self) -> String;
    fn to_camel_case(&self) -> String;
    fn to_snake_case(&self) -> String;
}

impl<T: AsRef<str>> StringExt for T {
    fn capitalize(&self) -> String {
        let mut chars = self.as_ref().split("").map(|s| s.to_string()).collect::<Vec<_>>();
        if chars.is_empty() {
            return String::new()
        }
        chars[0] = chars[0].to_uppercase();
        chars.join("")
    }
    fn to_camel_case(&self) -> String {
        let mut words = self.as_ref().split("_").map(|s| s.to_string()).collect::<Vec<_>>();
        let mut result = vec![];
        while let Some(first) = words.first() {
            if first == "_" {
                result.push("_".to_string());
                words.remove(0);
            } else {
                break;
            }
        }
        for (idx, word) in words.iter().enumerate() {
            if idx == 0 {
                result.push(word.to_lowercase())
            } else {
                result.push(word.to_lowercase().capitalize())
            }
        }
        result.join("")
    }
    fn to_snake_case(&self) -> String {
        let mut result = String::new();
        let mut in_prefix = true;
        let mut to_upper = false;
        for ch in self.as_ref().chars() {
            if ch == '_' {
                if in_prefix {
                    result += ch.to_string().as_str();
                } else {
                    to_upper = true;
                }
            } else {
                in_prefix = false;
            }
            if to_upper {
                result += ch.to_string().to_uppercase().as_str()
            } else {
                result += ch.to_string().as_str();
            }
        }
        result
    }
}

pub trait IntoOptionalType {
    fn into_optional_type(&self, typedb: &Api) -> Option<Type>;
}

impl IntoOptionalType for TsUnionType {
    fn into_optional_type(&self, typedb: &Api) -> Option<Type> {
        let types: Vec<_> = self.types
            .iter()
            .filter(|t| match t.as_ref() {
                TsType::TsKeywordType(TsKeywordType { span: _, kind: TsKeywordTypeKind::TsNullKeyword }) => false,
                TsType::TsKeywordType(TsKeywordType { span: _, kind: TsKeywordTypeKind::TsUndefinedKeyword }) => false,
                _ => true
            })
            .collect();
        if types.len() == 1 {
            Some(typedb.resolve_type(types[0].as_ref()))
        } else {
            None
        }
    }
}

pub trait TsTypeLitExtension {
    fn is_object_map(&self) -> bool;
    fn resolve_object_map(&self, typedb: &Api) -> Type;
    fn is_anonymous_object(&self) -> bool;
    fn resolve_anonymous_object(&self, typedb: &Api) -> Type;
}

impl TsTypeLitExtension for TsTypeLit {
    fn is_object_map(&self) -> bool {
        if self.members.len() != 1 {
            return false;
        }
        let sig = &self.members[0];
        if let TsTypeElement::TsIndexSignature(_) = sig {
            true
        } else {
            false
        }
    }

    fn is_anonymous_object(&self) -> bool {
        if self.members.len() > 1 {
            return true;
        }
        let sig = &self.members[0];
        if let TsTypeElement::TsIndexSignature(_) = sig {
            false
        } else {
            true
        }

    }

    fn resolve_object_map(&self, typedb: &Api) -> Type {
        if self.members.len() != 1 {
            panic!("Only know how to make ObjectMap from TsTypeLit, got {self:#?}")
        }
        let sig = &self.members[0];
        let TsTypeElement::TsIndexSignature(sig) = sig else {
            panic!("Unsupported TsTypeLit, on TsIndexSignature for keys is valid, got: {sig:#?}")
        };
        if sig.params.len() != 1 {
            panic!("Invalid signature for ObjectMap: {sig:#?}")
        };
        let param = &sig.params[0];
        let TsFnParam::Ident(param) = param else {
            panic!("Invalid signature for ObjectMap: {sig:#?}")
        };
        let key_type = typedb.resolve_type(param.type_ann.as_ref().unwrap().type_ann.as_ref());
        // panic!("{key_type:?}, {t:#?}");
        let value_type = typedb.resolve_type(sig.type_ann.as_ref().unwrap().type_ann.as_ref());
        Type::ObjectMap(Box::new(key_type), Box::new(value_type))
    }

    fn resolve_anonymous_object(&self, typedb: &Api) -> Type {
        let mut fields = vec![];
        for member in self.members.iter() {
            let TsTypeElement::TsPropertySignature(sig) = member else {
                println!("Non-property based anonymous object signature found for {self:#?}");
                return Type::None
            };
            let key = sig.key.as_ident().unwrap().sym.to_string();
            let ty = typedb.resolve_type(&sig.type_ann.as_ref().unwrap().type_ann);
            fields.push((key, ty))
        }
        Type::Object(fields)
        
    }
}


pub trait TsEntityNameExtension {
    fn into_type(&self, typedb: &Api, params: Vec<Type>) -> Type;
}
impl TsEntityNameExtension for TsEntityName {
    fn into_type(&self, _typedb: &Api, params: Vec<Type>) -> Type {
        let mut parts = vec![];
        let mut this = self;
        loop {
            match this {
                TsEntityName::TsQualifiedName(name) => {
                    parts.push(name.right.sym.to_string());
                    this = &name.left;
                }
                TsEntityName::Ident(ident) => {
                    parts.push(ident.sym.to_string());
                    parts.reverse();
                    break;
                }
            }
        }
        if parts.len() == 1 {
            Type::Reference(parts.into_iter().next().unwrap(), params)
        } else {
            Type::Path(parts)
        }
    }
}

pub trait TsTypeParamDeclExtension {
    fn into_type_params(&self, api: &Api) -> Vec<(String, Type)>;
}

impl TsTypeParamDeclExtension for Option<Box<TsTypeParamDecl>> {
    fn into_type_params(&self, api: &Api) -> Vec<(String, Type)> {
        if let Some(decl) = self {
            decl.into_type_params(api)
        } else {
            vec![]
        }
    }
}

impl TsTypeParamDeclExtension for TsTypeParamDecl {
    fn into_type_params(&self, api: &Api) -> Vec<(String, Type)> {
        self.params
            .iter()
            .map(|p| {
                let name = p.name.sym.to_string();
                let constraint = if let Some(c) = &p.constraint {
                    api.resolve_type(&c)
                } else {
                    Type::None
                };
                (name, constraint)
            })
            .collect()
    }
}