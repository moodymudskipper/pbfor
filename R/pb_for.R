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
#' pb_for()
#' for (i in 1:10) {
#'   # DO SOMETHING
#'   Sys.sleep(0.5)
#' }
#'
#' pb_for(format = "Working hard: [:bar] :percent :elapsed",
#'        callback = function(x) message("Were'd done!"))
#' for (i in 1:10) {
#'   # DO SOMETHING
#'   Sys.sleep(0.5)
#' }
pb_for <-
  function(
    # all args of progress::progress_bar$new() except `total` which needs to be
    # infered from the 2nd argument of the `for` call, and `stream` which is
    # deprecated
    format = "[:bar] :percent",
    width = options("width")[[1]] - 2,
    complete = "=",
    incomplete = "-",
    current =">",
    callback = invisible, # doc doesn't give default but this seems to work ok
    clear = TRUE,
    show_after = .2,
    force = FALSE,
    # The only arg not forwarded to progress::progress_bar$new()
    # By default `for` will self detruct after being called
    once = TRUE) {

    # create the function that will replace `for`
    f <- function(it, seq, expr){
      # to avoid notes at CMD check
      `*pb*` <- IT <- SEQ <- EXPR <- NULL

      # forward all arguments to progress::progress_bar$new() and add
      # a `total` argument compted from `seq` argument
      pb <- progress::progress_bar$new(
        format = format, width = width, complete = complete,
        incomplete = incomplete, current = current,
        callback = callback,
        clear = clear, show_after = show_after, force = force,
        total = length(seq))

      # using on.exit allows us to self destruct `for` if relevant even if
      # the call fails.
      # It also allows us to send to the local environment the changed/created
      # variables in their last state, even if the call fails (like standard for)
      on.exit({
        vars <- setdiff(ls(env), c("*pb*"))
        list2env(mget(vars,envir = env), envir = parent.frame())
        if(once) rm(`for`,envir = parent.frame())
      })

      # we build a regular `for` loop call with an updated loop code including
      # progress bar.
      # it is executed in a dedicated environment and the progress bar is given
      # a name unlikely to conflict
      env <- new.env(parent = parent.frame())
      env$`*pb*` <-  pb
      eval(substitute(
        env = list(IT = substitute(it), SEQ = substitute(seq), EXPR = substitute(expr)),
        base::`for`(IT, SEQ,{
          EXPR
          `*pb*`$tick()
        })), envir = env)
    }
    # override `for` in the parent frame
    assign("for", value = f,envir = parent.frame())
  }

#' Feed a progress bar to a for loop
#'
#' By using `for<-` we allow the syntax
#' `pb -> for(it in seq) expr` where `pb` is created by a call to
#' `progress::progress_bar$new()`. The; only restriction compared to a regular
#' `for` call (or a `for` call modified by `pb_for()`is that `int` must exist
#' and must not be `NULL`.
#'
#' @param it variable used to iterate
#' @param seq values to iterate over
#' @param expr expression to loop over
#' @param value progress bar built with `progress::progress_bar$new(...)`
#'
#' @export
`for<-` <-
  function(it, seq, expr, value){
    # to avoid notes at CMD check
    `*pb*` <- IT <- SEQ <- EXPR <- NULL
    # the symbol fed to `it` is unknown, R uses `*tmp*` for assignment functions
    # so we go get it by inspecting the memory addresses
    it_chr <- fetch_name(it)
    it_sym <-as.symbol(it_chr)

    #  complete the progress bar with the `total` parameter
    # we need to clone it because progress bars are environments and updated
    # by reference
    pb <- value$clone()
    pb$.__enclos_env__$private$total <- length(seq)

    # when the script ends, even with a bug, the values that have been changed
    # are writtent to the parent frame
    on.exit({
      vars <- setdiff(ls(env), c("*pb*"))
      list2env(mget(vars, env),envir = parent.frame())
    })

    # computations are operated in a separate environment so we don't pollute it
    # with it, seq, expr, value, we need the progress bar so we name it `*pb*`
    # unlikely to conflict by accident
    env <- new.env(parent = parent.frame())
    env$`*pb*` <-  pb
    eval(substitute(
      env =  list(IT = it_sym, SEQ = substitute(seq), EXPR = substitute(expr)),
      base::`for`(IT, SEQ,{
        EXPR
        `*pb*`$tick()
      })), envir = env)

    # because of the `fun<-` syntax we need to return the modified first argument
    invisible(get(it_chr,envir = env))
  }


fetch_name <- function(x,env = parent.frame(2)) {
  all_addresses       <- sapply(ls(env), address2, env)
  all_addresses       <- all_addresses[names(all_addresses) != "*tmp*"]
  all_addresses_short <- gsub("(^|<)[0x]*(.*?)(>|$)","\\2",all_addresses)

  x_address       <- tracemem(x)
  untracemem(x)
  x_address_short <- tolower(gsub("(^|<)[0x]*(.*?)(>|$)","\\2",x_address))

  ind    <- match(x_address_short, all_addresses_short)
  x_name <- names(all_addresses)[ind]
  x_name
}


address2 <- getFromNamespace("address2", "pryr")
