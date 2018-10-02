#' 将个税数据写入工资表
#'
#' @param data 符合模板的个税表数据 
#'
#' @return 返回值
#' @export
#'
#' @examples writePTCIntoTable(mydata);
writePTCIntoTable <- function(data) {
  NameCn <-c('姓名',
                     '部门',
                     '岗位',
                     '出勤天数',
                     '基本工资',
                     '年资',
                     '全勤奖',
                     '安全奖',
                     '房补',
                     '绩效',
                     '回款考核',
                     '补贴',
                     '其他工资',
                     '应发小计',
                     '养老',
                     '公积金',
                     '个税',
                     '其他扣款',
                     '扣款小计',
                     '实发工资',
                     '备注');
  NameEn <-c( "empName",
              "empDept",
              "empPosition",
              "attendances",
              "baseAmt",
              "seniorityAmt",     
              "fullAttendanceAmt",
              "safetyAmt",
              "houseSubsAmt",     
              "performanceAmt",    
              "collectionAmt",     
              "allowanceAmt",     
              "otherAmt", 
              "grossSalaryAmt",    
              "minusPensionAmt",  
              "minusAccumFundAmt",
              "minusPTAmt",
              "minusOtherAmt",    
              "minusTotalAmt",
              "netPayrollAmt",     
              "empNote" );
 
  data <- as.data.frame(data);
  #将数据从tibble调整为data.frame
  names(data) <- NameEn;
  data$grossSalaryAmt <-data$baseAmt+
    data$seniorityAmt+
    data$fullAttendanceAmt+
    data$safetyAmt+
    data$houseSubsAmt+
    data$performanceAmt+
    data$collectionAmt+
    data$allowanceAmt+
    data$otherAmt;
  minusTotalAmtBeforeTax <-data$minusPensionAmt+
    data$minusAccumFundAmt+
    data$minusOtherAmt;
  preTaxAmt <- data$grossSalaryAmt -minusTotalAmtBeforeTax;
  data$minusPTAmt <- getTaxOutput(preTaxAmt = preTaxAmt,threshold = 5000)$`个税`;
  data$minusTotalAmt <-data$minusPensionAmt+
    data$minusAccumFundAmt+
    data$minusOtherAmt+
    data$minusPTAmt;
  data$netPayrollAmt <- data$grossSalaryAmt - data$minusTotalAmt;
  names(data) <- NameCn
  return(data);
  
}