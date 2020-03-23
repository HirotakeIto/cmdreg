#' R6 Class representing a Regression analysis environment
#'
#' A environment have `analyze` method.
#' @export
RegressionAnalysisEnvironment = R6::R6Class(
  "RegressionAnalysisEnvironment",
  lock_objects = FALSE,
  public = list(
    #' @field status represents status of analysis.
    status = "before_analyze",

    #' @field messeage represents messeage of analysis.
    messeage = character(),

    #' @field result is object representing result of analysis.
    result = list(),

    #' @field document is Document.
    document = "このクラスのドキュメント",

    #' @field tag is the list of tag representing this object.
    tag = c(character()),

    #' @description
    #' Return the object name.
    #' @return character string representing this object.
    name = function(){paste(self$tag, collapse = '_', sep = '_')},

    #' @description
    #' Create a new RegressionAnalysisEnvironment object.
    #' @param ... arguments becoming object attribute.
    #' @return A new `RegressionAnalysisEnvironment` object.
    initialize = function(...){
      args = list(...)
      for (name in names(args)) {self[[name]] = args[[name]]}
    },

    #' @description
    #' attributuion setter.
    #' @param name attributuion name.
    #' @param value attributuion value
    add_attr = function(name, value) self[[name]] = value,

    #' @description
    #' this function is base function for analysis. You should override this function at the child class.
    analyze_base = function() {print("Please Override")},

    #' @description
    #' this function is base function for analysis. You should override this function at the child class.
    #' @param error_stop logical. If error_stop is TRUE, this function will stop when any error happens.
    #' @param ... arguments becoming variable of `analyze_base` function.
    analyze = function(error_stop=TRUE, ...){
      tryCatch({
        res = self$analyze_base(...)
        for (name in names(res)) {self$result[[name]] = res[[name]]}  # func_analyzeの結果を自らに格納
        self$status = 'analyzed'
      },
      # warningの場合の挙動を理解していないので修正の余地あり
      error = function(e) {
        self$status = 'fail'
        message(e)
        self$messeage = e$message
      },
      silent = FALSE
      )
    },

    # setter and getter for formula
    fm_ = formula(),
    get_fm = function() self$fm_,
    set_fm = function(x) self$fm_ = x,
    fm_str = character()
  ),
  private = list()
)
