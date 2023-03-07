#' @title Launch an RStudio addin which allows to schedule an Rscript interactively.
#' @description Launch an RStudio addin which allows to schedule an Rscript interactively.
#' 
#' @param RscriptRepository path to the folder where R scripts will be copied to and launched from, and by default log files will be written to.
#' Defaults to the current working directory or in case it is set, the path set in the \code{CRON_LIVE} environment variable.
#' @return the return of \code{\link[shiny]{runGadget}}
#' @export
#' @examples 
#' \dontrun{
#' cron_rstudioaddin()
#' }
cron_rstudioaddin <- function(RscriptRepository = Sys.getenv("CRON_LIVE", unset = getwd())) {
  
  cron_current <- function(){
    x <- try(parse_crontab(), silent = TRUE)
    if(inherits(x, "try-error")){
      x <- list(cronR = character())
    }
    x
  }
  requireNamespace("cronR")
  requireNamespace("shiny")
  requireNamespace("miniUI")
  requireNamespace("shinyFiles")

  check <- NULL
  
  popup <- shiny::modalDialog(title = "重要提醒",
                              "你在使用棱星公司的同步服务,请安全使用系统服务及R语言脚本",
                              shiny::tags$br(),
                              shiny::tags$br(),
                              shiny::modalButton("同意", icon = shiny::icon("play")),
                              shiny::actionButton(inputId = "ui_validate_no", label = "不同意退出", icon = shiny::icon("stop")),
                              footer = NULL,
                              easyClose = FALSE)
  
  ui <- miniUI::miniPage(
 
    miniUI::gadgetTitleBar("棱星数据定时任务自定义ID(R版)"),
    
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(title = '创建新任务', icon = shiny::icon("cloud-upload"),
                           miniUI::miniContentPanel(
                             #shiny::uiOutput('fileSelect'),
                             shiny::h4("请选择R语言脚本"),
                             shinyFiles::shinyFilesButton('fileSelect', label='选择文件', title='请选择一下R语言脚本', multiple=FALSE),
                             shiny::br(),
                             shiny::br(),
                             shiny::fillRow(flex = c(3, 3),
                                            shiny::column(6,
                                                          shiny::div(class = "control-label", shiny::strong("选中的R脚本")),
                                                          shiny::verbatimTextOutput('currentfileselected'),
                                                          shiny::textInput('jobid', label = "任务ID标识(系统自动生成)", value = "job_id"),
                                                         
                                                          shiny::dateInput('date', label = "生效日期:", startview = "month", weekstart = 1, min = Sys.Date()),
                                                          
                                                          shiny::radioButtons('task', label = "执行频率:", choices = c('一次性', '每分钟一次', '每小时一次', '每天一次', '每周一次', '每月一次', '自定义'), selected = "一次性"),
                                                          shiny::textInput('custom_schedule', label = "自定义cronTab输入", value = "")
                                            ),
                                            shiny::column(6,
                                                          shiny::textInput('rscript_args', label = "脚本参数(可选)", value = ""),
                                                         
                                                          shiny::textInput('jobdescription', label = "定时任务描述", value = "此任务自动同步..."),
                                                          shiny::textInput('hour', label = "生效时间:", value = format(Sys.time() + 122, "%H:%M")),
                                                          shiny::textInput('jobtags', label = "任务标签", value = ""),
                                                          
                                                          
                                                          shiny::textInput('rscript_repository', label = "任务日志目录设置", value = RscriptRepository)
                                            ))
                            
                           ),
                           miniUI::miniButtonBlock(border = "bottom",
                                                   shiny::actionButton('create', "创建定时任务", icon = shiny::icon("play-circle"))
                           )
      ),
      miniUI::miniTabPanel(title = '管理已有定时任务', icon = shiny::icon("table"),
                           miniUI::miniContentPanel(
                             shiny::fillRow(flex = c(3, 3),
                                            shiny::column(6,
                                                          shiny::h4("定时任务列表"),
                                                          shiny::actionButton('showcrontab', "显示当前任务", icon = shiny::icon("calendar")),
                                                          shiny::br(),
                                                          shiny::br(),
                                                          shiny::h4("选择一个任务"),
                                                          shiny::uiOutput("getFiles"),
                                                          shiny::actionButton('showjob', "显示任务详情", icon = shiny::icon("clock-o")),
                                                          shiny::actionButton('deletejob', "删除指定任务", icon = shiny::icon("remove"))
                                            ),
                                            shiny::column(6,
                                                          shiny::h4("保存任务至文件"),
                                                          shiny::textInput('savecrontabpath', label = "保存当前任务至cron文件", value = file.path(Sys.getenv("HOME"), "my_schedule.cron")),
                                                          shiny::actionButton('savecrontab', "保存为文件", icon = shiny::icon("save")),
                                                          shiny::br(),
                                                          shiny::br(),
                                                          shiny::h4("加载cron文件"),
                                                          #shiny::uiOutput('cronload'),
                                                          shinyFiles::shinyFilesButton('crontabSelect', label='选择一下cron文件', title='Select crontab schedule', multiple=FALSE),
                                                          #shiny::div(class = "control-label", strong("Selected crontab")),
                                                          shiny::br(),
                                                          shiny::br(),
                                                          shiny::actionButton('loadcrontab', "加载选择中的cron文件", icon = shiny::icon("load")),
                                                          shiny::br(),
                                                          shiny::br(),
                                                          shiny::verbatimTextOutput('currentcrontabselected')
                                            ))
                           ),
                           miniUI::miniButtonBlock(border = "bottom",
                                                   shiny::actionButton('deletecrontab', "清除所有计划任务", icon = shiny::icon("delete"))
                           )
      )
    )
  )
  
  # Server code for the gadget.
  server <- function(input, output, session) {
    shiny::showModal(popup)
    shiny::observeEvent(input$ui_validate_no, {
      shiny::stopApp()
    })
    
    volumes <- c('Current working dir' = getwd(), 'HOME' = Sys.getenv('HOME'), 'R Installation' = R.home(), 'Root' = "/")
    getSelectedFile <- function(inputui, default = "没有选中R脚本"){
      f <- shinyFiles::parseFilePaths(volumes, inputui)$datapath
      f <- as.character(f)
      if(length(f) == 0){
        return(default)
      }else{
        if(length(grep(" ", f, value=TRUE))){
          warning(sprintf("It is advised that the file you want to schedule (%s) does not contain spaces", f))
        }
      }
      f
    }
    # Ui element for fileinput
    shinyFiles::shinyFileChoose(input, id = 'fileSelect', roots = volumes, session = session)
    output$fileSelect <- shiny::renderUI({shinyFiles::parseFilePaths(volumes, input$fileSelect)})
    output$currentfileselected <- shiny::renderText({getSelectedFile(inputui = input$fileSelect)})
    #output$fileSelect <- shiny::renderUI({
    #  shiny::fileInput(inputId = 'file', 'Choose your Rscript',
    #            accept = c("R-bestand"),
    #            multiple = TRUE)
    #})
    shinyFiles::shinyFileChoose(input, id = 'crontabSelect', roots = volumes, session = session)
    output$crontabSelect <- shiny::renderUI({shinyFiles::parseFilePaths(volumes, input$crontabSelect)})
    output$currentcrontabselected <- shiny::renderText({basename(getSelectedFile(inputui = input$crontabSelect, default = ""))})
    # output$cronload <- shiny::renderUI({
    #   shiny::fileInput(inputId = 'crontabschedule', 'Load an existing crontab schedule & overwrite current schedule',
    #                    multiple = FALSE)
    # })
    
    # when path to Rscript repository has been changed, check for existence of path and write permissions,
    # and normalize RscriptRepository path in parent environment.
    shiny::observeEvent(input$rscript_repository, {
      RscriptRepository <<- normalizePath(input$rscript_repository, winslash = "/")
      verify_rscript_path(RscriptRepository)
    })
    
    ###########################
    # CREATE / OVERWRITE
    ###########################
    shiny::observeEvent(input$create, {
      shiny::req(input$task)
      #shiny::req(input$file)
      
      if(input$task == "每月一次" ){
        days <- as.integer(format(input$date, "%d"))
      }
      else if(input$task == "每周一次"){
        days <- as.integer(format(input$date, "%w"))
      }
      else {
        # get default value by setting days to null.
        days <- NULL
      }
      starttime <- input$hour
      rscript_args <- input$rscript_args
      frequency <- factor(input$task, 
                          levels = c('一次性', '每分钟一次', '每小时一次', '每天一次', '每周一次', '每月一次', "自定义"),
                          labels = c('once', 'minutely', 'hourly', 'daily', 'weekly', 'monthly', 'asis'))
      frequency <- as.character(frequency)
      
      ##
      ## Copy the uploaded file from the webapp to the main folder to store the scheduled rscripts.
      ##
      if(length(grep(" ", RscriptRepository)) > 0){
        warning(sprintf("It is advised that the RscriptRepository does not contain spaces, change argument %s to another location on your drive which contains no spaces", RscriptRepository))
      }
      
      if (!file.exists(RscriptRepository)) {
        stop(sprintf("The specified Rscript repository path, at %s, does not exist. Please set it to an existing directory.", RscriptRepository))
      }
      
      runme <- getSelectedFile(inputui = input$fileSelect)
      myscript <- paste0(RscriptRepository, "/", basename(runme))
      print(myscript)
      if(runme != myscript){
        done <- file.copy(runme, myscript, overwrite = TRUE)
        if(!done){
          stop(sprintf('Copying file %s to %s failed. Do you have access rights to %s?', file.path(runme, input$file$name), myscript, dirname(myscript)))
        }  
      }
      ##
      ## Make schedule task
      ##
      cmd <- sprintf("Rscript %s %s >> %s.log 2>&1", myscript, rscript_args, tools::file_path_sans_ext(myscript))
      cmd <- sprintf('%s %s %s >> %s 2>&1', file.path(Sys.getenv("R_HOME"), "bin", "Rscript"), shQuote(myscript), rscript_args, shQuote(sprintf("%s.log", tools::file_path_sans_ext(myscript))))
      if(frequency %in% c('minutely')){
        cron_add(command = cmd, frequency = frequency, id = input$jobid, tags = input$jobtags, description = input$jobdescription, ask=FALSE)  
      }else if(frequency %in% c('hourly')){
        cron_add(command = cmd, frequency = frequency, at = starttime, id = input$jobid, tags = input$jobtags, description = input$jobdescription, ask=FALSE)  
      }else if(frequency %in% c('daily')){
        cron_add(command = cmd, frequency = 'daily', at = starttime, id = input$jobid, tags = input$jobtags, description = input$jobdescription, ask=FALSE)  
      }else if(frequency %in% c('weekly')){
        cron_add(command = cmd, frequency = 'daily', days_of_week = days, at = starttime, id = input$jobid, tags = input$jobtags, description = input$jobdescription, ask=FALSE)  
      }else if(frequency %in% c('monthly')){
        cron_add(command = cmd, frequency = 'monthly', days_of_month = days, days_of_week = 1:7, at = starttime, id = input$jobid, tags = input$jobtags, description = input$jobdescription, ask=FALSE)  
      }else if(frequency %in% c('once')){
        message(sprintf("This is not a cron schedule but will launch: %s", sprintf('nohup %s &', cmd)))
        system(sprintf('nohup %s &', cmd))
      }else if(frequency %in% c('asis')){
        cron_add(command = cmd, frequency = input$custom_schedule, id = input$jobid, tags = input$jobtags, description = input$jobdescription, ask=FALSE)  
      }
      
      # Reset ui inputs
      shiny::updateDateInput(session, inputId = 'date', value = Sys.Date())
      shiny::updateTextInput(session, inputId = "hour", value = format(Sys.time() + 122, "%H:%M"))
      shiny::updateRadioButtons(session, inputId = 'task', selected = "一次性")
      shiny::updateTextInput(session, inputId = "jobid", value = "job_id")
      shiny::updateTextInput(session, inputId = "jobdescription", value = "定时任务描述")
      shiny::updateTextInput(session, inputId = "jobtags", value = "")
      shiny::updateTextInput(session, inputId = "rscript_args", value = "")
      # output$fileSelect <- shiny::renderUI({
      #   shiny::fileInput(inputId = 'file', 'Choose your Rscript',
      #                    accept = c("R-bestand"),
      #                    multiple = TRUE)
      # })
      #output$currentfileselected <- shiny::renderText({""})
      shiny::updateSelectInput(session, inputId="getFiles", choices = sapply(cron_current()$cronR, FUN=function(x) x$id))
    })
    
    ###########################
    # Schedule list
    ###########################
    output$getFiles <- shiny::renderUI({
      shiny::selectInput(inputId = 'getFiles', "选择一个任务", choices = sapply(cron_current()$cronR, FUN=function(x) x$id))
    })
    ###########################
    # Show
    ###########################
    shiny::observeEvent(input$showcrontab, {
      cron_ls()
    })
    shiny::observeEvent(input$showjob, {
      cron_ls(input$getFiles)
    })
    ###########################
    # Save/Load/Delete
    ###########################
    shiny::observeEvent(input$savecrontab, {
      message(input$savecrontabpath)
      cron_save(file = input$savecrontabpath, overwrite = TRUE)
    })
    shiny::observeEvent(input$loadcrontab, {
      #cron_load(file = input$crontabschedule$datapath)
      f <- getSelectedFile(inputui = input$crontabSelect, default = "")
      message(f)
      if(f != ""){
        cron_load(file = f, ask=FALSE)
      }
      output$getFiles <- shiny::renderUI({
        shiny::selectInput(inputId = 'getFiles', "Select job", choices = sapply(cron_current()$cronR, FUN=function(x) x$id))
      })
    })
    shiny::observeEvent(input$deletecrontab, {
      cron_clear(ask = FALSE)
      output$getFiles <- shiny::renderUI({
        shiny::selectInput(inputId = 'getFiles', "Select job", choices = sapply(cron_current()$cronR, FUN=function(x) x$id))
      })
    })
    shiny::observeEvent(input$deletejob, {
      cron_rm(input$getFiles, ask=FALSE)
      output$getFiles <- shiny::renderUI({
        shiny::selectInput(inputId = 'getFiles', "Select job", choices = sapply(cron_current()$cronR, FUN=function(x) x$id))
      })
    })
    
    
    
    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    shiny::observeEvent(input$done, {
      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      shiny::stopApp()
    })
  }
  
  # Use a modal dialog as a viewr.
  viewer <- shiny::dialogViewer("Cron job scheduler", width = 700, height = 800)
  #viewer <- shiny::paneViewer()
  shiny::runGadget(ui, server, viewer = viewer)
}

verify_rscript_path <- function(RscriptRepository) {
  # first check whether path exists; if it does, then check whether you have write permission.
  if(is.na(file.info(RscriptRepository)$isdir)){
    warning(sprintf("The specified Rscript repository path %s does not exist, make sure this is an existing directory without spaces.", RscriptRepository))
  } else if (as.logical(file.access(RscriptRepository, mode = 2))) {
    warning(sprintf("You do not have write access to the specified Rscript repository path, %s.", RscriptRepository))
  }
}
