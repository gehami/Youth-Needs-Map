############ Done. Below is junk ##############

######### Setting up prediction for gp_sub - gp_sub_dat ###########
if(identical(dep_dat$GEOID, big_dat$GEOID) & #GEOIDs are in the same order
   identical((dep_dat$start_date), big_dat$start_date)){ #you can actually just cbind them.
  gp_sub_dat = data.frame(big_dat[,-which(colnames(big_dat) %in% c('GEOID', 'start_date', 'end_date'))], dep_gp_sub = dep_dat$gp_and_sub)
}else{message('dep_dat and big_dat did not match up for some reason. Check yourself')}

########### Conducting a random forest prediction for gp_sub ############

install_and_load('randomForest')
install_and_load('tree')
set.seed(95116)
train = sample(1:nrow(gp_sub_dat), nrow(gp_sub_dat)/2)
# tree.gp_sub = tree(dep_gp_sub ~ ., data = gp_sub_dat, subset = train)
# cv.gp_sub = cv.tree(tree.gp_sub)
# plot(cv.gp_sub$size, cv.gp_sub$dev, type = 'b')

gp_sub.rf = randomForest(dep_gp_sub ~ ., data = gp_sub_dat, subset = train, mtry = ceiling(sqrt(ncol(gp_sub_dat))))
yhat.rf = predict(gp_sub.rf, newdata = gp_sub_dat[-train,])
y.test = gp_sub_dat[-train,]$dep_gp_sub
print(mean(abs(yhat.rf - y.test), na.rm = T)) #we are hitting an average accuracy within a %age point
print(mean((yhat.rf - y.test)^2, na.rm = T))

importance(gp_sub.rf)
varImpPlot(gp_sub.rf)

#saving model
saveRDS(gp_sub.rf, 'gp_sub_1_year_rf.rds')



######### Setting up prediction for vocational_training - vocational_dat ###########
if(identical(dep_dat$GEOID, big_dat$GEOID) & #GEOIDs are in the same order
   identical((dep_dat$start_date - DAYS_FORWARD), big_dat$start_date)){ #you can actually just cbind them.
  vocational_dat = data.frame(big_dat[,-which(colnames(big_dat) %in% c('GEOID', 'start_date', 'end_date'))], dep_vocational = dep_dat$vocational_training)
}else{message('dep_dat and big_dat did not match up for some reason. Check yourself')}

########### Conducting a random forest prediction for vocational ############

install_and_load('randomForest')
install_and_load('tree')
set.seed(95116)
train = sample(1:nrow(vocational_dat), nrow(vocational_dat)/2)
# tree.vocational = tree(dep_vocational ~ ., data = vocational_dat, subset = train)
# cv.vocational = cv.tree(tree.vocational)
# plot(cv.vocational$size, cv.vocational$dev, type = 'b')

vocational.rf = randomForest(dep_vocational ~ ., data = vocational_dat, subset = train, mtry = ceiling(sqrt(ncol(vocational_dat))))
yhat.rf = predict(vocational.rf, newdata = vocational_dat[-train,])
y.test = vocational_dat[-train,]$dep_vocational
print(mean(abs(yhat.rf - y.test), na.rm = T)) #we are hitting an average accuracy within  0.1%age points
print(mean((yhat.rf - y.test)^2, na.rm = T))

importance(vocational.rf)
varImpPlot(vocational.rf)

#saving model
saveRDS(vocational.rf, 'vocational_1_year_rf.rds')

######### Setting up prediction for parent_training - parent_dat ###########
if(identical(dep_dat$GEOID, big_dat$GEOID) & #GEOIDs are in the same order
   identical((dep_dat$start_date - DAYS_FORWARD), big_dat$start_date)){ #you can actually just cbind them.
  parent_dat = data.frame(big_dat[,-which(colnames(big_dat) %in% c('GEOID', 'start_date', 'end_date'))], dep_parent = dep_dat$parent_awareness)
}else{message('dep_dat and big_dat did not match up for some reason. Check yourself')}

########### Conducting a random forest prediction for parent ############

install_and_load('randomForest')
install_and_load('tree')
set.seed(95116)
train = sample(1:nrow(parent_dat), nrow(parent_dat)/2)
# tree.parent = tree(dep_parent ~ ., data = parent_dat, subset = train)
# cv.parent = cv.tree(tree.parent)
# plot(cv.parent$size, cv.parent$dev, type = 'b')

parent.rf = randomForest(dep_parent ~ ., data = parent_dat, subset = train, mtry = ceiling(sqrt(ncol(parent_dat))))
yhat.rf = predict(parent.rf, newdata = parent_dat[-train,])
y.test = parent_dat[-train,]$dep_parent
print(mean(abs(yhat.rf - y.test), na.rm = T)) #we are hitting an average accuracy within 0.3%age points
print(mean((yhat.rf - y.test)^2, na.rm = T))

importance(parent.rf)
varImpPlot(parent.rf)

#saving model
saveRDS(parent.rf, 'parent_1_year_rf.rds')

