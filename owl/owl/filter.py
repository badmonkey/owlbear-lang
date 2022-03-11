from .token import Kind, OwlbearToken
from pipey import Pipeable


@Pipeable
def strip_undents(src):
    stash = None

    for t in src:
        if t.kind == Kind.UNDENT:
            if stash:
                stash.kind = Kind.END
                yield stash
            stash = t
        elif t.kind in [Kind.END]:
            # drop UNDENT and emit the real scope ending
            stash = None
            yield t
        else:
            if stash:
                stash.kind = Kind.END
                yield stash
                stash = None
            yield t


@Pipeable
def fix_bracketed_scope(src):
    working = []
    count = 0

    for t in src:
        match t.kind:
            case Kind.LPAREN:
                count += 1
                working.append(Kind.LPAREN)
                yield t

            case Kind.RPAREN:
                if count == 0:
                    yield t
                    continue

                if working[-1] == Kind.LPAREN:
                    count -= 1
                    working.pop()
                    yield t
                else:
                    yield OwlbearToken(Kind.BAD,
                                       t.text,
                                       line = t.line,
                                       lineno = t.lineno,
                                       column = t.column,
                                       text=f"Error while tracking '(' found {t}")
                    yield t

            case Kind.BEGIN:
                if count > 1:
                    # only worry about scope if we're already matching atleast 1 parens
                    working.append(Kind.BEGIN)
                    continue
                yield t

            case Kind.END:
                if count == 0:
                    yield t
                    continue

                if working[-1] == Kind.BEGIN:
                    working.pop()
                else:
                    yield OwlbearToken(Kind.BAD,
                                       t.text,
                                       line = t.line,
                                       lineno = t.lineno,
                                       column = t.column,
                                       text=f"Error while tracking '{{' found {t}")
                    yield t

            case _:
                yield t


@Pipeable
def strip_newlines(src):
    for t in src:
        if t.kind not in [Kind.NEWLINE, Kind.ENDMARK]:
            yield t


def pipeline(src):
    return (src >> strip_undents
                >> fix_bracketed_scope
                >> strip_newlines)





# class FilterState(Enum):
#         PASS_TOKEN = 1
#         REPLACE_UNDENT = 2
#         WAIT_CLOSE_BRACKET = 3
#         WAIT_UNDENT = 4
#         SQUASH_NEWLINES = 5
#         SQUASH_COMMENTS = 6


# END_SCOPE_TOKENS = ['END', 'CATCH', 'FINALLY']





# class foo:
#     def filtered_tokens(self):
#         state = [FilterState.PASS_TOKEN]
#         stash_undent = None

#         for t in self.tokens:
#             match state[-1]:
#                 case FilterState.PASS_TOKEN:
#                     match t.kind:
#                         case 'LPAREN':
#                             state.append(FilterState.WAIT_CLOSE_BRACKET)
#                         case 'NEWLINE':
#                             state.append(FilterState.SQUASH_NEWLINES)
#                         case 'UNDENT':
#                             stash_undent = t
#                             state.append(FilterState.REPLACE_UNDENT)
#                             print("DEBUG", t)
#                             continue
#                         case _:
#                             pass
#                     yield t

#                 case FilterState.REPLACE_UNDENT:
#                     if t.kind not in END_SCOPE_TOKENS:
#                         match t.kind:
#                            case 'UNDENT':
#                                stash_undent.kind = TOKEN_END
#                                yield stash_undent
#                                stash_undent = t
#                                continue

#                            case 'NEWLINE':
#                                # todo ignore newlines for the moment
#                                continue

#                            case _: # todo check and process tokens
#                                # convert the UNDENT into an END
#                                stash_undent.kind = TOKEN_END
#                                yield stash_undent

#                     state.pop()
#                     stash_undent = None
#                     yield t

#                 case FilterState.WAIT_CLOSE_BRACKET:
#                     match t.kind:
#                         case 'RPAREN':
#                             state.pop()
#                             yield t
#                         case 'LPAREN':
#                             state.append(FilterState.WAIT_CLOSE_BRACKET)
#                             yield t
#                         case 'BEGIN':
#                             state.append(FilterState.WAIT_UNDENT)
#                             # drop the BEGIN
#                         case _:
#                             yield t

#                 case FilterState.WAIT_UNDENT:
#                     match t.kind:
#                         case 'UNDENT':
#                             state.pop()
#                             # drop the UNDENT
#                         case 'BEGIN':
#                             state.append(FilterState.WAIT_UNDENT)
#                             # drop the BEGIN
#                         case 'LPAREN':
#                             state.append(FilterState.WAIT_CLOSE_BRACKET)
#                             yield t
#                         case _:
#                             yield t

#                 case FilterState.SQUASH_NEWLINES:
#                     if t.kind != TOKEN_NEWLINE:
#                         state.pop()
#                         yield t   # todo check token first

#                 case FilterState.SQUASH_COMMENTS:
#                     pass
