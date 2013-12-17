#lang pyret

import ast as A

provide *

fun normalize-term(M):
  normalize(M, fun(x): x;)
end

fun normalize-name(M, k):
  l = error.location("", 0, 0)
  normalize(M, fun(N):
      if A.is-s_bool(N) or A.is-s_num(N) or A.is-s_str(N) or A.is-s_id(N):
        k(N)
      else:
        t = gensym("temp")
        A.s_block(l, [
            A.s_var(l, A.s_bind(l, t, A.a_blank), N),
            k(A.s_id(l, t))
          ])
      end
    end)
end

fun normalize-name-rec(M, k):
  l = error.location("", 0, 0)
  cases (List) M:
    | empty => k([])
    | link(fst, rst) =>
      normalize-name(fst, fun(t):
          normalize-name-rec(rst, fun(t-rec):
              k(link(t, t-rec))
            end)
        end)
  end
end

fun normalize-program(M):
  cases (List) M:
    | empty => empty
    | link(fst, rst) => link(normalize-term(fst), normalize-program(rst))
  end
end

fun normalize(M, k):
  cases (A.Expr) M:

    | s_block(l, stmts) =>
      k(A.s_block(l, normalize-program(stmts)))

    | s_user_block(l, body) =>
      k(A.s_user_block(l, normalize-term(body)))

    | s_let(l, name, value) =>
      k(A.s_let(l, name, normalize-term(value)))

    | s_var(l, name, value) =>
      k(A.s_var(l, name, normalize-term(value)))

    | s_assign(l, id, value) =>
      k(A.s_assign(l, id, normalize-term(value)))

    | s_if_else(l, branches, _else) =>
      s-if = for fold(acc from _else, branch from branches):
        A.s_if_else(l, [branch], acc)
      end

      if A.is-s_if_else(s-if):
        cond = s-if.branches.first.test
        consq = s-if.branches.first.body
        altern = s-if._else
        normalize-name(cond, fun(t):
            k(A.s_if_else(l, [A.s_if_branch(l, t, normalize-term(consq))], normalize-term(altern)))
          end)
      else:
        k(normalize-term(s-if))
      end

    | s_try(l, body, id, _except) =>
      normalize-name(body, fun(t-body):
          normalize-name(_except, fun(t-except):
              k(A.s_try(l, t-body, id, t-except))
            end)
        end)
      
    | s_lam(l, params, args, ann, doc, body, _check) =>
      k(A.s_lam(l, params, args, ann, doc, normalize-term(body), _check))

    | s_method(l, args, ann, doc, body, _check) =>
      k(A.s_method(l, args, ann, doc, normalize-term(body), _check))

    | s_extend(l, super, fields) =>
      names = fields.map(fun(field): field.name;)
      exprs = fields.map(fun(field): field.value;)
      
      normalize-name(super, fun(t):
          normalize-name-rec(exprs, fun(t-rec):
              k(A.s_extend(l, t, for fold2(acc from [],
                                           name from names.reverse(),
                                           nexpr from t-rec.reverse()):
                    link(A.s_data_field(l, name, nexpr), acc)
                  end))
            end)
        end)
      
    | s_obj(l, fields) =>
      names = fields.map(fun(field): field.name;)
      exprs = fields.map(fun(field): field.value;)

      normalize-name-rec(exprs, fun(t-rec):
          k(A.s_obj(l, for fold2(acc from [],
                                 name from names.reverse(),
                                 nexpr from t-rec.reverse()):
                link(A.s_data_field(l, name, nexpr), acc)
              end))
        end)

    | s_app(l, _fun, args) =>
      normalize-name(_fun, fun(t):
          normalize-name-rec(args, fun(t-rec):
              k(A.s_app(l, t, t-rec))
            end)
        end)

    | s_id(_, _) => k(M)
      
    | s_num(_, _) => k(M)

    | s_bool(_, _) => k(M)
      
    | s_str(_, _) => k(M)

    | s_bracket(l, obj, field) =>
      normalize-name(obj, fun(t-obj):
          normalize-name(field, fun(t-field):
              k(A.s_bracket(l, t-obj, t-field))
            end)
        end)

    | s_colon_bracket(l, obj, field) =>
      normalize-name(obj, fun(t-obj):
          normalize-name(field, fun(t-field):
              k(A.s_colon_bracket(l, t-obj, t-field))
            end)
        end)

    | s_get_bang(l, obj, field) =>
      normalize-name(obj, fun(t):
          k(A.s_get_bang(l, t, field))
        end)

    | s_update(l, super, fields) =>
      names = fields.map(fun(field): field.name;)
      exprs = fields.map(fun(field): field.value;)
      
      normalize-name(super, fun(t):
          normalize-name-rec(exprs, fun(t-rec):
              k(A.s_update(l, t, for fold2(acc from [],
                                           name from names.reverse(),
                                           nexpr from t-rec.reverse()):
                    link(A.s_data_field(l, name, nexpr), acc)
                  end))
            end)
        end)
      
    | else => raise("NYI")
  end
end