import ast
from json.decoder import JSONDecodeError
from sys import stdin
import json


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
    try:
        ast_json = json.loads("".join(stdin.readlines()))
        a = ast.fix_missing_locations(gen_ast(ast_json))
        print(ast.unparse(a))
        print("BEGIN EVALUATION")
        eval(compile(a, "", "exec"))
        print("END EVALUATION")
    except JSONDecodeError:
        print("got empty/invalid json")

