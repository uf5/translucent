import sys, json
import ast


def gen_ast(obj):
    match obj:
        case int(x) | float(x) | str(x):
            return x

        case None:
            return None

        case list(x):
            return [gen_ast(e) for e in x]

        case _:
            return (
                getattr(ast, obj["tag"])
                (**{e:gen_ast(obj[e])
                    for e in obj
                    if e != "tag"})
            )


if __name__ == "__main__":
    with open(sys.argv[1], "r") as json_file:
        ast_json = json.load(json_file)
        print(ast_json)
        a = ast.fix_missing_locations(gen_ast(ast_json))
        print("AST:")
        print(ast.dump(a))
        print("RECONSTRUCTED:")
        print(ast.unparse(a))
        print("BEGIN EVALUATION")
        eval(compile(a, "", "exec"))
        print("END EVALUATION")

