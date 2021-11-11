
class symbol(str):
    pass


class string(str):
    pass


def str_of_sexp(sexp):
    def sexp_to_str(sexp):
        if isinstance(sexp, list):
            return '(' + ' '.join(map(sexp_to_str, sexp)) + ')'
        elif isinstance(sexp, bool):
            return '#t' if sexp else '#f'
        elif isinstance(sexp, symbol):
            return sexp
        elif isinstance(sexp, string):
            return '"' + repr(sexp)[1:-1] + '"'
        elif isinstance(sexp, int):
            return str(sexp)
        elif isinstance(sexp, float):
            return str(sexp)
        else:
            assert False, "Can't deal with {}".format(repr(sexp))
    try:
        return sexp_to_str(sexp)
    except AssertionError as e:
        print(str(sexp))
        raise e
