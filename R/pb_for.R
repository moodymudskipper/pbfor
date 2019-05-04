#' Use a progress bar with regular for loops
#'
#' A wrapper around the package *progress* to be able to use progress bar with
#' regular `for` loops conveniently. It forwards all its parameters to
#' `progress::progress_bar$new()` except for `once`.
#'
#' @param format The format of the progress bar. A number of tokens can be used
#'   here, see them below. It defaults to "[:bar] :percent", which means that
#'   the progress bar is within brackets on the left, and the percentage is
#'   printed on the right.
#' @param width Width of the progress bar.
#' @param complete Completion character.
#' @param incomplete Incomplete character.
#' @param current Current character.
#' @param callback Callback function to call when the progress bar finishes.
#'   The progress bar object itself is passed to it as the single parameter.
#' @param clear Whether to clear the progress bar on completion.
#' @param show_after Amount of time in seconds, after which the progress bar is
#'   shown on the screen. For very short processes, it is probably not worth
#'   showing it at all.
#' @param force Whether to force showing the progress bar, even if the given (or default) stream does not seem to support it.
#' @param once Wether to return `for` to it's default behavior after it's been
#'   called once.
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'   pb_for()
#'   for (i in 1:10) {
#'     # DO SOMETHING
#'     Sys.sleep(0.5)
#'   }
#'
#'   pb_for(format = "Working hard: [:bar] :percent :elapsed", callback = ~message("Were'd done!"))
#'   for (i in 1:10) {
#'     # DO SOMETHING
#'     Sys.sleep(0.5)
#'   }
#' }
pb_for <-
  function(format = "[:bar] :percent",
           width = options("width")[[1]] - 2,
           complete = "=",
           incomplete = "-",
           current =">",
           callback = invisible,
           clear = TRUE,
           show_after = .2,
           force = FALSE,
           once = TRUE) {
    f <- function(`*it*`, `*seq*`, `*expr*`){
      `*pb*` <- progress::progress_bar$new(
        format = format, width = width, complete = complete,
        incomplete = incomplete, current = current,
        callback = rlang::as_function(callback),
        clear = clear, show_after = show_after, force = force,
        total = length(`*seq*`))
      on.exit({
        vars <- setdiff(ls(), c("*pb*", "*expr*", "*it*", "*seq*"))
        list2env(mget(vars),envir = parent.frame())
        if(once) rm(`for`,envir = parent.frame())
      })
      eval(substitute(
        env = list(IT = substitute(`*it*`), SEQ = substitute(`*seq*`), EXPR = substitute(`*expr*`)),
        base::`for`(IT, SEQ,{
          EXPR
          `*pb*`$tick()
        })))
    }
    assign("for", value = f,envir = parent.frame())
  }




#
# pb <- function(format = "[:bar] :percent",
#                width = options("width")[[1]] - 2,
#                complete = "=",
#                incomplete = "-",
#                current =">",
#                callback = invisible,
#                clear = TRUE,
#                show_after = .2,
#                force = FALSE){
#   structure(list(format = format, width = width, complete = complete,
#                  incomplete = incomplete, current = current,
#                  callback = rlang::as_function(callback),
#                  clear = clear, show_after = show_after, force = force),
#             class = "forpb_input")
# }
#
#
# `for<-` <- function(it, seq, expr, value) UseMethod("for<-")
#
# `for<-.forpb_input` <-
#   function(it, seq, expr, value){
#     value$total <- length(seq)
#     `*pb*` <-  do.call(progress::progress_bar$new, value)
#       eval(substitute(
#         env = list(it = substitute(it), seq = substitute(seq), expr = substitute(expr)),
#         base::`for`(it, seq,{
#           expr
#           `*pb*`$tick()
#         })))
#       if(once) rm(`for`,envir = parent.frame())
#     }
#     assign("for", value = f,envir = parent.frame())
#   }

