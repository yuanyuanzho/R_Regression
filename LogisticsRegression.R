loan <-read.csv("/Users/eavy/Downloads/loan.csv")

res_status <- loan$Res_status
occupation <- loan$Occupation
job_status <- loan$Job_status
liab_ref <- loan$Liab_ref
acc_ref <- loan$Acc_ref
decision <- loan$Decision

myModel <- glm( decision ~ res_status + occupation + job_status + liab_ref + acc_ref, data = loan,family = binomial)
summary(myModel)

# prediction
test1 <- data.frame(res_status ="owner", occupation="creative_", job_status = "governmen", liab_ref="f", acc_ref="given")
predict1 <- predict(myModel, test1, type  = "response")

test2 <- data.frame(res_status ="rent", occupation="creative_", job_status = "governmen", liab_ref="f", acc_ref="given")
predict2 <- predict(myModel, test2, type = "response")




