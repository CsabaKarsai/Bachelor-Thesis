# ---- FUNCTIONS ----

#custom save
i3_saveFig <- function(plot, dir, filename, plot_width = 24, plot_height = 13.5, plot_scale = 0.5) {
  ggsave(plot = plot, paste0(dir, "/", filename, ".pdf"), width = plot_width * plot_scale, height = plot_height * plot_scale)
}

#custom theme
i3_generateTheme <- function(fontSize = 20, margin.top = 5, margin.bottom = 5, margin.left = 5, margin.right = 5) {
  font <- element_text(size = fontSize)
  mytheme <- theme(axis.text = font,
                   axis.title = font,
                   legend.text = font,
                   legend.title = element_blank(),
                   legend.position = "top",
                   plot.margin = margin(t = margin.top, b = margin.bottom, l = margin.left, r = margin.right))
  return(mytheme)
}

# Count, Mean, Variance, standard deviation, standard error, 95% Confidence Intervals for waitingTime
getStatisticsWaitingTime <- function(worker, loadFactor){
  messageTypes <- worker %>%
    group_by(messageType) %>%
    summarize(
      n = n(),
      mean = mean(waitingTime),
      var = var(waitingTime),
      sd = sd(waitingTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1),
      loadFactor = loadFactor
    )

  all <- worker %>%
    summarize(
      messageType = "all",
      n = n(),
      mean = mean(waitingTime),
      var = var(waitingTime),
      sd = sd(waitingTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1),
      loadFactor = loadFactor
    )

  bind_rows(messageTypes, all)
}

#NEU get mean waiting time of all workers and all messages of a dataset
getMeanWaitingTimeCurrentDataset <- function(loadFactor){
  tail(getStatisticsWaitingTime(allWorkers, loadFactor), 1) %>%
    summarize(
      meanWaitingTime = mean,
      ic = ic,
      loadFactor = loadFactor
    )
}

loadDataset("Baseline_Gamma", 4)
workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)
getMeanWaitingTimeCurrentDataset(1)

allWorkers <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95))
getMeanWaitingTimeCurrentDataset(1)

# Count, Mean, Variance, standard deviation, standard error, 95% Confidence Intervals for processingTime
getStatisticsProcessingTime <- function(worker, loadFactor){
  messageTypes <- worker %>%
    group_by(messageType) %>%
    summarize(
      n = n(),
      mean = mean(processingTime),
      var = var(processingTime),
      sd = sd(processingTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1),
      loadFactor = loadFactor
    )

  all <- worker %>%
    summarize(
      messageType = "all",
      n = n(),
      mean = mean(processingTime),
      var = var(processingTime),
      sd = sd(processingTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1),
      loadFactor = loadFactor
    )

  bind_rows(messageTypes, all)
}

#NEU get mean processingTime time of all workers and all messages of a dataset
getMeanProcessingTimeCurrentDataset <- function(loadFactor){
  tail(getStatisticsProcessingTime(allWorkers, loadFactor), 1) %>%
    summarize(
      meanProcessingTime = mean,
      ic = ic,
      loadFactor = loadFactor
    )
}

# Count, Mean, Variance, standard deviation, standard error, 95% Confidence Intervals for leadTime
getStatisticsSojournTime <- function(datasetAsString, worker, loadFactor){
  messageTypes <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    group_by(messageType) %>%
    mutate(sojournTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime) %>%
    summarize(
      n = n(),
      mean = mean(sojournTime),
      var = var(sojournTime),
      sd = sd(sojournTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1),
      loadFacotr = loadFactor
    )

  all <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(sojournTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime) %>%
    summarize(
      messageType = "all",
      n = n(),
      mean = mean(sojournTime),
      var = var(sojournTime),
      sd = sd(sojournTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1),
      loadFacotr = loadFactor
    )

  bind_rows(messageTypes, all)
}

#NEU get mean sojournTime time of all workers and all messages of a dataset
getMeanSojournTimeCurrentDataset <- function(datasetAsString, loadFactor){
  tail(getStatisticsSojournTime(datasetAsString, allWorkers, loadFactor), 1) %>%
    summarize(
      meanSojournTime = mean,
      ic = ic,
      loadFactor = loadFactor
    )
}

# Count, Mean, Variance, standard deviation, standard error, 95% Confidence Intervals for interArrival times
getStatisticsInterarrivalTimes <- function(data, loadFactor){
  wrongFirstRow <- data %>%
    arrange(messageArriveTime) %>%
    mutate(iat = messageArriveTime - lag(messageArriveTime, 1, NA))

  wrongFirstRow[-1,] %>%
    summarize(
      n = n(),
      mean = mean(iat),
      var = var(iat),
      sd = sd(iat),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1),
      loadFactor = loadFactor
    )
}

getStatisticsInterarrivalTimes(allWorkers, 1)

#NEU get mean interArrival time of all workers and all messages of a dataset
getMeanIatCurrentDataset <- function(loadFactor){
  getStatisticsInterarrivalTimes(allWorkers, loadFactor) %>%
    summarize(
      meanIatTime = mean,
      ic = ic,
      loadFactor = loadFactor
    )
}

# plot mean waitingTime for each message type with 95% CI
plotMeanWaitingTimes <- function(worker){
  messageTypes <- worker %>%
    group_by(messageType) %>%
    summarize(
      n=n(),
      mean=mean(waitingTime),
      sd=sd(waitingTime)
    ) %>%
    mutate(se=sd/sqrt(n))  %>%
    mutate(ic=se * qt(0.975, n-1))

  all <- worker %>%
    summarize(
      messageType = "all",
      n=n(),
      mean=mean(waitingTime),
      sd=sd(waitingTime)
    ) %>%
    mutate(se=sd/sqrt(n))  %>%
    mutate(ic=se * qt(0.975, n-1))

  ggplot(data = messageTypes, mapping = aes(x = messageType, y = mean)) +
    geom_bar(stat = "identity", fill = cbPalette[8]) +
    geom_errorbar(aes(x=messageType, ymin=mean-ic, ymax=mean+ic), width = 0.4, size = 0.5) +
    geom_hline(aes(yintercept = (all$mean)), linetype = "dashed", size = 0.5) +
    xlab("Message Types") +
    ylab("Waiting Time [microseconds]")
}

#plot mean waiting times filtered by 95% quantile
plotMeanWaitingTimesFiltered <- function(worker){
  dataFiltered <- worker %>% filter(waitingTime < quantile(waitingTime, 0.95)) %>%
    mutate(messageArriveTime = messageArriveTime - min(messageArriveTime))

  plotMeanWaitingTimes(dataFiltered)
}

#plot waiting times in relation to time
plotWaitingTimes <- function(worker){
  ggplot(data = worker, mapping = aes(x = messageArriveTime / 1000000, y = waitingTime, color = messageType)) +
    geom_line() +
    xlab("Time [seconds]") +
    ylab("Waiting Time [microseconds]") +
    i3_generateTheme()
}

# plot mean processingTime for each message type with 95% CI
plotMeanProcessingTimes <- function(data){
  messageTypes <- data %>%
    group_by(messageType) %>%
    summarize(
      n=n(),
      mean=mean(processingTime),
      sd=sd(processingTime)
    ) %>%
    mutate(se=sd/sqrt(n))  %>%
    mutate(ic=se * qt(0.975, n-1))

  all <- data %>%
    summarize(
      messageType = "all",
      n=n(),
      mean=mean(processingTime),
      sd=sd(processingTime)
    ) %>%
    mutate(se=sd/sqrt(n))  %>%
    mutate(ic=se * qt(0.975, n-1))

  ggplot(data = messageTypes, mapping = aes(x = messageType, y = mean)) +
    geom_bar(stat = "identity", fill = cbPalette[8]) +
    geom_errorbar(aes(x=messageType, ymin=mean-ic, ymax=mean+ic), width = 0.4, size = 0.5) +
    geom_hline(aes(yintercept = (all$mean)), linetype = "dashed", size = 0.5) +
    xlab("Message Types") +
    ylab("Processing Time [microseconds]")
}

# plot mediam waiting times
plotMedianWaitingTimes <- function(worker){
  messageTypes <- worker %>%
    group_by(messageType) %>%
    summarize(
      n=n(),
      median=median(waitingTime),
      sd=sd(waitingTime)
    ) %>%
    mutate(se=sd/sqrt(n))  %>%
    mutate(ic=se * qt(0.975, n-1))

  all <- worker %>%
    summarize(
      messageType = "all",
      n=n(),
      median=median(waitingTime),
      sd=sd(waitingTime)
    ) %>%
    mutate(se=sd/sqrt(n))  %>%
    mutate(ic=se * qt(0.975, n-1))

  ggplot(data = messageTypes, mapping = aes(x = messageType, y = median)) +
    geom_bar(stat = "identity", fill = cbPalette[8]) +
    geom_hline(aes(yintercept = (all$median)), linetype = "dashed", size = 0.5) +
    xlab("Message Types") +
    ylab("Waiting Time [microseconds]")
}

# plot mean sojournTime for each message type with 95% CI
plotMeanSojournTimes <- function(datasetAsString, worker){
  messageTypes <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    group_by(messageType) %>%
    mutate(leadTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime) %>%
    summarize(
      n = n(),
      mean = mean(leadTime),
      var = var(leadTime),
      sd = sd(leadTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1)
    )

  all <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(leadTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime) %>%
    summarize(
      messageType = "all",
      n = n(),
      mean = mean(leadTime),
      var = var(leadTime),
      sd = sd(leadTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1)
    )

  ggplot(data = messageTypes, mapping = aes(x = messageType, y = mean)) +
    geom_bar(stat = "identity", fill = cbPalette[8]) +
    geom_errorbar(aes(x=messageType, ymin=mean-ic, ymax=mean+ic), width = 0.4, size = 0.5) +
    geom_hline(aes(yintercept = (all$mean)), linetype = "dashed", size = 0.5) +
    xlab("Message Types") +
    ylab("Sojourn Time [microseconds]")
}

# plot mean sojournTime for each message type with 95% CI
plotMeanSojournTimesFiltered <- function(datasetAsString, worker){
  messageTypes <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    group_by(messageType) %>%
    mutate(leadTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime) %>%
    filter(leadTime < quantile(leadTime, 0.95)) %>%
    summarize(
      n = n(),
      mean = mean(leadTime),
      var = var(leadTime),
      sd = sd(leadTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1)
    )

  all <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(leadTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime) %>%
    filter(leadTime < quantile(leadTime, 0.95)) %>%
    summarize(
      messageType = "all",
      n = n(),
      mean = mean(leadTime),
      var = var(leadTime),
      sd = sd(leadTime),
      se = sd/sqrt(n),
      ic = se * qt(0.975, n-1)
    )

  ggplot(data = messageTypes, mapping = aes(x = messageType, y = mean)) +
    geom_bar(stat = "identity", fill = cbPalette[8]) +
    geom_errorbar(aes(x=messageType, ymin=mean-ic, ymax=mean+ic), width = 0.4, size = 0.5) +
    geom_hline(aes(yintercept = (all$mean)), linetype = "dashed", size = 0.5) +
    xlab("Message Types") +
    ylab("Sojourn Time [microseconds]")
}

#NEU plot sojournTime in relation to time
plotSojournTimes <- function(datasetAsString, worker){
  data <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(sojournTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime,
           SupervisorSendTime = SupervisorSendTime - min(SupervisorSendTime))

  ggplot(data = data %>% sample_n(size = nrow(data) * 0.1), mapping = aes(x = (SupervisorSendTime / 1000000), y = sojournTime)) +
    geom_line() +
    xlab("Time [seconds]") +
    ylab("Soujourn Time [microseconds]")
}

#plot sojournTime in relation to time filtered
plotSojournTimesFiltered <- function(datasetAsString, worker){
  data <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(sojournTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime,
           SupervisorSendTime = SupervisorSendTime - min(SupervisorSendTime)) %>%
    filter(sojournTime < quantile(sojournTime, 0.95))

  ggplot(data = data %>% sample_n(size = nrow(data) * 0.1), mapping = aes(x = (SupervisorSendTime / 1000000), y = sojournTime)) +
    geom_line() +
    xlab("Time [seconds]") +
    ylab("Soujourn Time [microseconds]")
}

#NEU plot sojournTimes in relation to time with binning
plotSojournTimesBinningFacetGrid <- function(datasetAsString, worker){
  binSize <- 1000000

  data <- worker %>%
    arrange(messageArriveTime) %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(sojournTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime,
           messageArriveTime = messageArriveTime - min(messageArriveTime),
           bin = floor(messageArriveTime / binSize) * binSize) %>%
    summarize(bin = bin,
              n = n(),
              binSojournTime = sum(sojournTime, na.rm = TRUE) / n,
              workerId = workerId)

  ggplot(data = data %>% sample_n(size = nrow(data) * 0.1), mapping = aes(x = (bin / 1000000), y = binSojournTime)) +
    geom_line() +
    facet_grid(rows = vars(workerId)) +
    xlab("Time [seconds]") +
    ylab("Sojourn Time [microseconds]")
}

#NEU plot sojournTime in relation to time
plotSojournTimesXlim <- function(datasetAsString, worker, xlim){
  data <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(sojournTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime,
           SupervisorSendTime = SupervisorSendTime - min(SupervisorSendTime))

  ggplot(data = data, mapping = aes(x = (SupervisorSendTime / 1000000), y = sojournTime)) +
    geom_line() +
    xlab("Time [seconds]") +
    xlim(0, xlim) +
    ylab("Soujourn Time [microseconds]")
}

#NEU plot sojournTime facet_grid
plotSojournTimesFacetGrid <- function(datasetAsString){
  data <- allWorkers %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(sojournTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime,
           SupervisorSendTime = SupervisorSendTime - min(SupervisorSendTime))

  ggplot(data = data %>% sample_n(size = nrow(data) * 0.1), mapping = aes(x = (SupervisorSendTime / 1000000), y = sojournTime)) +
    geom_line() +
    facet_grid(rows = vars(workerId)) +
    xlab("Time [seconds]") +
    #ylim(0, 9000) +
    ylab("Soujourn Time [microseconds]")
}

#NEU plot IatTimes in relation to time
plotInterarrivalTimes <- function(data){
  data <- data %>%
    arrange(messageArriveTime) %>%
    mutate(messageArriveTime = messageArriveTime - min(messageArriveTime),
           iat = messageArriveTime - lag(messageArriveTime, 1, NA))

  data <- data[-1,]

  ggplot(data = data, mapping = aes(x = (messageArriveTime / 1000000), y = iat / 1000)) +
    geom_line() +
    xlab("Time [seconds]") +
    ylab("Interarrival Time [microseconds]")
}

#NEU plot interarrival time with binning
plotInterarrivalTimesBinning <- function(data){
  binSize <- 1000000

  data <- data %>%
    arrange(messageArriveTime) %>%
    mutate(messageArriveTime = messageArriveTime - min(messageArriveTime),
           iat = messageArriveTime - lag(messageArriveTime, 1, NA),
           bin = floor(messageArriveTime / binSize) * binSize) %>%
    group_by(bin) %>%
    summarize(bin = bin,
              n = n(),
              binIat = sum(iat, na.rm = TRUE) / n)

  ggplot(data = data, mapping = aes(x = (bin / 1000000), y = binIat / 1000)) +
    geom_line() +
    xlab("Time [seconds]") +
    ylab("Interarrival Time [milliseconds]")
}

#percentage of messages below max acceptable processing time
getStatisticsPctFailedMessages <- function(datasetAsString, worker, maxTime, loadFactor){
  worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(totalTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime,
           timeout = ifelse(totalTime < maxTime, "ok", "failed")) %>%
    group_by(timeout) %>%
    summarize(n = n()) %>%
    mutate(pct = n/ sum(n)) %>%
    summarize(timeout = timeout,
              pct = pct,
              loadFactor = loadFactor)
}

#NEU get pct failed messages of all workers and all messages of a dataset
getPctFailedCurrentDataset <- function(datasetAsString, maxTime, loadFactor){
  head(getStatisticsPctFailedMessages(datasetAsString, allWorkers, maxTime, loadFactor), 1)
}

#plot pct failed messages
plotPctFailedMessages <- function(datasetAsString, worker, maxTime, loadFactor){
  dataToPlot <- getStatisticsPctFailedMessages(datasetAsString, worker, maxTime, loadFactor)

  ggplot(data = dataToPlot, mapping = aes(x = timeout, y = pct * 100)) +
    geom_bar(stat = "identity", fill = cbPalette[8]) +
    xlab("Timeout") +
    ylab("Percentage of failed messages") +
    ylim(0, 100)
}

#CDF waiting Times
plotCdfWaitingTimes <- function(worker){
  ggplot(data = worker, mapping = aes(x = waitingTime)) +
    stat_ecdf(geom = "step", size = 1) +
    geom_vline(aes(xintercept = mean(waitingTime)), linetype = "dashed", size = 1) +
    xlab("Waiting Time [microseconds]") +
    xlim(0, 50) +
    ylab("ECDF")
}

#CDF waiting Times with colors
plotCdfWaitingTimesColors <- function(worker){
  ggplot(data = worker, mapping = aes(x = waitingTime, color = messageType)) +
    stat_ecdf(geom = "step", size = 1) +
    geom_vline(aes(xintercept = mean(waitingTime)), linetype = "dashed", size = 1) +
    xlab("Waiting Time [microseconds]") +
    #xlim(0, 50) +
    ylab("ECDF")
}

# CDF waiting Times with colors filtered by 95% quantile
plotCdfWaitingTimesColorsfiltered <- function(worker){
  data <- worker %>% filter(waitingTime < quantile(waitingTime, 0.95)) %>%
    mutate(messageArriveTime = messageArriveTime - min(messageArriveTime))

  ggplot(data = data, mapping = aes(x = waitingTime, color = messageType)) +
    stat_ecdf(geom = "step", size = 1) +
    geom_vline(aes(xintercept = mean(waitingTime)), linetype = "dashed", size = 1) +
    xlab("Waiting Time [microseconds]") +
    #xlim(0, 40) +
    ylab("ECDF")
}

#PDF waiting Times
plotPdfWaitingTimes <- function(worker){
  ggplot(data = worker, mapping = aes(x = waitingTime, ..scaled..)) +
    geom_density(geom = "step", size = 1) +
    geom_vline(aes(xintercept = mean(waitingTime)), linetype = "dashed", size = 1) +
    xlab("Waiting Time [microseconds]") +
    xlim(0, 50) +
    ylab("Probability")
}

#CDF processing Times
plotCdfProcessingTimes <- function(worker){
  ggplot(data = worker, mapping = aes(x = processingTime)) +
    stat_ecdf(geom = "step", size = 1) +
    geom_vline(aes(xintercept = mean(processingTime)), linetype = "dashed", size = 1) +
    stat_function(fun=pgamma,color="orange",args=list(shape=10.659899772,scale=(1 / 0.009338315))) +
    xlab("Processing Time [milliseconds]") +
    xlim(0, 5000) +
    ylab("Probability")
}

#PDF processing Times
plotPdfProcessingTimes <- function(worker){
  ggplot(data = worker, mapping = aes(x = processingTime, ..scaled..)) +
    geom_density(geom = "step", size = 1) +
    geom_vline(aes(xintercept = mean(processingTime)), linetype = "dashed", size = 1) +
    xlab("Processing Time [microseconds]") +
    xlim(0, 5000) +
    ylab("Probability")
}

#utilization of a single worker in relation to time
getUtilizationSingleWorker <- function(worker){
  binSize <- 1000000

  worker %>%
    mutate(messageArriveTime = (messageArriveTime - min(messageArriveTime)),
           bin = floor(messageArriveTime / binSize) * binSize) %>%
    group_by(bin) %>%
    summarize(messageArriveTime = mean(messageArriveTime) / 1000000,
              utilization = sum(processingTime) / 1000000)
}

#mean utilization of a single worker
getMeanUtilizationSingleWorker <- function(worker){
  getUtilizationSingleWorker(worker) %>%
    summarize(meanUtilization = mean(utilization))
}

#plot utilization of single worker in relation to time
plotUtilizationSingleWorker <- function(worker){
  ggplot(data = utilizationSKS(worker), mapping = aes(x = bin / 1000000, y = utilization)) +
    geom_line() +
    xlab("Time [seconds]") +
    ylab("Percentage of Utilization")
}

plotUtilizationSingleWorker(Baseline_Gamma_30xLoad_w1)

#utilization of system in relation to time
getUtilizationSystem <- function(){
  data <- getUtilizationSingleWorker(workerList[[1]]) %>%
    summarize(bin = bin,
              utilization = utilization)

  for(i in 2:length(workerList)){
    data <- data %>%
      left_join((getUtilizationSingleWorker(workerList[[i]]) %>%
                   summarize(bin = bin,
                             utilization = utilization)), by = "bin")
  }

  data %>%
    summarize(bin = bin,
              utilization = rowMeans(data[,c(2,3,4,5)], na.rm = TRUE))
}

#mean utilization of system
getMeanUtilizationSystem <- function(loadFactor){
  getUtilizationSystem() %>%
    summarize(meanSystemUtilization = mean(utilization, na.rm = TRUE),
              loadFactor = loadFactor)
}

#plot utilization of system in relation to time
plotUtilizationSystem <- function(){
  ggplot(data = getUtilizationSystem(), mapping = aes(x = bin / 1000000, y = utilization)) +
    geom_line() +
    xlab("Time [seconds]") +
    xlim(0, 1000) +
    ylab("Percentage of Utilization")
}

#offer single worker in relation to time
getOfferSingleWorker <- function(worker){
  getUtilizationSingleWorker(worker) %>%
    summarize(bin = bin,
              offer = utilization)
}

#mean offer os single worker
getMeanOfferSingleWorker <- function(worker){
  getOfferSingleWorker(worker) %>%
    summarize(meanOffer = mean(offer))
}

#plot offer of single worker in relation to time
plotOfferSingleWorker <- function(worker){
  ggplot(data = getOfferSingleWorker(worker), mapping = aes(x = bin / 1000000, y = offer)) +
    geom_line() +
    xlab("Time [seconds]") +
    ylab("Offer [Erlang]")
}

#offer of system in relation to time
getOfferSystem <- function(workerCount){
  data <- getOfferSingleWorker(workerList[[1]]) %>%
    summarize(bin = bin,
              offer = offer)

  for(i in 2:length(workerList)){
    data <- data %>%
      left_join((getOfferSingleWorker(workerList[[i]]) %>%
                   summarize(bin = bin,
                             offer = offer)), by = "bin")
  }

  data %>%
    summarize(bin = bin,
              offer = rowMeans(data[,c(2,3,4,5)]) / workerCount)
}

#mean offer of system
getMeanOfferSystem <- function(workerCount, loadFactor){
  getOfferSystem(workerCount) %>%
    summarize(meanSystemOffer = mean(offer, na.rm = TRUE),
              loadFactor = loadFactor)
}

#plot offer of system in relation to time
plotOfferSystem <- function(workerCount){
  ggplot(data = getOfferSystem(workerCount), mapping = aes(x = bin / 1000000, y = offer)) +
    geom_line() +
    xlab("Time [seconds]") +
    ylab("Offer")
}

#function to set working directory to a dataset and concatenate datasets
setWdToDataset <- function(datasetAsString){
  setwd(paste0("C:/Users/csaba/IdeaProjects/akka-benchmark/results/", datasetAsString))
}

#function to load  library, dataset and allWorkers
loadDataset <- function(datasetAsString, workerCount){
  setwd(paste0("C:/Users/csaba/IdeaProjects/akka-benchmark/results/", datasetAsString))

  assign(paste0(datasetAsString, "_", "supervisor"), read_delim(paste0("Supervisor.txt"), delim = ";") %>%
           mutate(workerId = "supervisor"), envir = parent.frame())

  allWorkers <<- NULL

  for(i in 1:workerCount){
    allWorkers <<- rbind(
      allWorkers,
      assign(paste0(datasetAsString, "_w", i), read_delim(paste0("Worker", i, ".txt"), delim = ";") %>%
               mutate(workerId = i), envir = parent.frame())
    )
  }

  # cb-colours: c(black,      grey,      lorange,   lblue,     green,     yellow,    dblue,     dorange,   pink )
  cbPalette <<- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  myDarkBlue <<- "#000099"

  red <<- "#CC0000"
}

#function to cut data
cutData <- function(data, minMessageArriveTimeInSeconds){
  min <- as.numeric(data[1, 2])
  newData <<- filter(data, messageArriveTime - min > (minMessageArriveTimeInSeconds * 1000000))
  newData
}

#functions to plot metrics according to list of datasets

#queue size

#number of all messages in the system

#Littles Law

# ---- Code for Analysis ----

#load library
library(tidyverse) #for utilization/offer system functions
library(scales)

#EVALUATION ----
loadDataset("Baseline_Gamma", 4)
workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)

#B A S E L I N E

#PROCESSING TIMES
#nicht jeden worker einzeln, da sehr aehnlich
getStatisticsProcessingTime(allWorkers, 1)
e1 <- plotMeanProcessingTimes(allWorkers) + i3_generateTheme()
i3_saveFig(e1, "C:/BA/Evaluation", "1_Baseline_meanProcessingTimes")

e2 <- plotCdfProcessingTimes(allWorkers) + i3_generateTheme()
i3_saveFig(e2, "C:/BA/Evaluation", "2_Baseline_ecdfProcessingTimes")

getStatisticsProcessingTime(allWorkers, 1)
e3 <- plotMeanProcessingTimes(Baseline_Gamma_supervisor) + i3_generateTheme()
i3_saveFig(e3, "C:/BA/Evaluation", "3_Baseline_meanProcessingTimesSupervisor")

#WAITING TIMES
#nicht jeden worker einzeln, da sehr aehnlich
getStatisticsWaitingTime(allWorkers, 1)
e4 <- plotMeanWaitingTimes(allWorkers) + i3_generateTheme()
i3_saveFig(e4, "C:/BA/Evaluation", "4_Baseline_meanWaitingTimes")

plotMeanWaitingTimesFiltered(allWorkers) + i3_generateTheme()
i3_saveFig(last_plot, "C:/BA/Evaluation", "f4_Baseline_meanWaitingTimesFiltered")

#f4 filtered 0.95 quantile
getStatisticsWaitingTime(allWorkers, 1)

dataFiltered <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95)) %>%
  mutate(messageArriveTime = messageArriveTime - min(messageArriveTime))

plotMeanWaitingTimesFiltered(allWorkers) + i3_generateTheme()
i3_saveFig(last_plot(), "C:/BA/Evaluation", "f4_Baseline_meanWaitingTimesFiltered")

#f4 median waiting times
plotMedianWaitingTimes(allWorkers) + i3_generateTheme()
i3_saveFig(last_plot(), "C:/BA/Evaluation", "f4_plotMedianWaitingTimes")

#f4 zeitreihe original
plotWaitingTimes(allWorkers) + i3_generateTheme(margin.left = 25, margin.right = 25)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "f4_zeitreiheOriginal")

#f4 cdf ungefilterte daten
plotCdfWaitingTimesColors(allWorkers)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "f4_plotCdfWaitingTimesColored")

#e5 old
#getStatisticsWaitingTime(allWorkers, 1)
#e5 <- plotCdfWaitingTimes(allWorkers) + i3_generateTheme()
#i3_saveFig(e5, "C:/BA/Evaluation", "5_Baseline_ecdfWaitingTimes")

#e5 new
getStatisticsWaitingTime(allWorkers, 1)
e5 <- plotCdfWaitingTimesColors(allWorkers) + i3_generateTheme()
i3_saveFig(e5, "C:/BA/Evaluation", "5_Baseline_ecdfWaitingTimesColors")

#f5 with data filtered by 95% quantile
plotCdfWaitingTimesColorsfiltered(allWorkers) + i3_generateTheme()
i3_saveFig(last_plot(), "C:/BA/Evaluation", "f5_plotCdfWaitingTimesColorsFiltered")

#analysis of CL and UL measurements
loadDataset("CL", 1)
loadDataset("UL", 1)

CL_normed <- CL_w1 %>%
  mutate(messageArriveTime = messageArriveTime - min(messageArriveTime))

UL_normed <- UL_w1 %>%
  mutate(messageArriveTime = messageArriveTime - min(messageArriveTime))

CLUL <- rbind(CL_normed, UL_normed)

cut_CLUL <- cutData(CLUL, 10)

getStatisticsWaitingTime(cut_CLUL, "special")

e6 <- ggplot(data = cut_CLUL, mapping = aes(x = messageArriveTime / 1000000, y = waitingTime, color = messageType)) +
  geom_line() +
  xlab("Time [seconds]") +
  ylab("Waiting Time [microseconds]") +
  i3_generateTheme()

i3_saveFig(e6, "C:/BA/Evaluation", "6_Baseline_Special_CLULWaitingTimes")

#baseline gamma random pool
loadDataset("Baseline_Gamma_RandomPool", 4)

Baseline_Gamma_RandomPool_supervisor <- na.omit(Baseline_Gamma_RandomPool_supervisor)
Baseline_Gamma_RandomPool_w1 <- na.omit(Baseline_Gamma_RandomPool_w1)
Baseline_Gamma_RandomPool_w2 <- na.omit(Baseline_Gamma_RandomPool_w2)
Baseline_Gamma_RandomPool_w3 <- na.omit(Baseline_Gamma_RandomPool_w3)
Baseline_Gamma_RandomPool_w4 <- na.omit(Baseline_Gamma_RandomPool_w4)
allWorkers <- rbind(Baseline_Gamma_RandomPool_w1, Baseline_Gamma_RandomPool_w2, Baseline_Gamma_RandomPool_w3, Baseline_Gamma_RandomPool_w4)

workerList <- list(Baseline_Gamma_RandomPool_w1, Baseline_Gamma_RandomPool_w2, Baseline_Gamma_RandomPool_w3, Baseline_Gamma_RandomPool_w4)

getStatisticsWaitingTime(allWorkers, 1)
e7 <- plotMeanWaitingTimes(allWorkers) + i3_generateTheme()
i3_saveFig(e7, "C:/BA/Evaluation", "7_Baseline_RandomPool_meanWaitingTime")

loadDataset("Baseline_Gamma", 4)
workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)

#SOJOURN TIMES
getStatisticsSojournTime("Baseline_Gamma", allWorkers, 1)
e8 <- plotMeanSojournTimes("Baseline_Gamma", allWorkers) + i3_generateTheme()
i3_saveFig(e8, "C:/BA/Evaluation", "8_Baseline_meanSojournTimes")

#f8 plotMeanSojournTimesFiltered
getStatisticsSojournTime("Baseline_Gamma", allWorkers, 1)
f8 <- plotMeanSojournTimesFiltered("Baseline_Gamma", allWorkers) + i3_generateTheme()
i3_saveFig(f8, "C:/BA/Evaluation", "f8_Baseline_meanSojournTimesFiltered")

#analysis of first message in system
e9 <- plotSojournTimes("Baseline_Gamma", Baseline_Gamma_w1) + i3_generateTheme(margin.left = 20, margin.right = 20)
i3_saveFig(e9, "C:/BA/Evaluation", "9_Baseline_plotSojournTimes")

cut10_Baseline_Gamma_w1 <- cutData(Baseline_Gamma_w1, 10)
e10 <- plotSojournTimesXlim("Baseline_Gamma", Baseline_Gamma_w1, 10) + i3_generateTheme()
i3_saveFig(e10, "C:/BA/Evaluation", "10_Baseline_plotSojournTimesXlim")i3_saveFig(plotSojournTimes("Baseline_Gamma", cut10_Baseline_Gamma_w1), "C:/BA/Evaluation", "13_Baseline_plotSojournTimesCut")

e11 <- plotSojournTimes("Baseline_Gamma", cut10_Baseline_Gamma_w1) + i3_generateTheme()
i3_saveFig(e11, "C:/BA/Evaluation", "11_Baseline_plotSojournTimesCut")

#analysis sojourn times
Baseline_Gamma_supervisor <- cutData(Baseline_Gamma_supervisor, 10)
cut_Baseline_Gamma_w1 <- cutData(Baseline_Gamma_w1, 10)
cut_Baseline_Gamma_w2 <- cutData(Baseline_Gamma_w2, 10)
cut_Baseline_Gamma_w3 <- cutData(Baseline_Gamma_w3, 10)
cut_Baseline_Gamma_w4 <- cutData(Baseline_Gamma_w4, 10)
workerList <- list(cut_Baseline_Gamma_w1, cut_Baseline_Gamma_w2, cut_Baseline_Gamma_w3, cut_Baseline_Gamma_w4)
e12 <- plotSojournTimesFacetGrid("Baseline_Gamma") + theme(strip.text.y = element_text(size = 20)) + i3_generateTheme(fontSize = 20)
i3_saveFig(e12, "C:/BA/Evaluation", "12_Baseline_plotSojournTimesFacetGrid")
loadDataset("Baseline_Gamma", 4)
workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)

#PCT FAILED MESSAGES
#artificial timeout = 2000
getStatisticsPctFailedMessages("Baseline_Gamma", allWorkers, 2000, "1")
plotPctFailedMessages("Baseline_Gamma", allWorkers, 2000, "1")
i3_saveFig(plotPctFailedMessages("Baseline_Gamma", allWorkers, 2000, "1"), "C:/BA/Evaluation", "15_Baseline_plotPctFailedMessages")

getStatisticsPctFailedMessages("Baseline_Gamma", allWorkers, 2000, "1")
e13 <- plotPctFailedMessages("Baseline_Gamma", allWorkers, 2000, "1") + i3_generateTheme()
i3_saveFig(e13, "C:/BA/Evaluation", "13_Baseline_Baseline_plotPctFailedMessages")

#INTERARRIVAL TIMES
getStatisticsInterarrivalTimes(allWorkers, 1)
plotInterarrivalTimesBinning(allWorkers)
i3_saveFig(plotInterarrivalTimesBinning(allWorkers), "C:/BA/Evaluation", "16_Baseline_plotInterarrivalTimesBinning")

getStatisticsInterarrivalTimes(allWorkers, 1)
e14 <- plotInterarrivalTimesBinning(allWorkers) + i3_generateTheme(margin.left = 20, margin.right = 20)
i3_saveFig(e14, "C:/BA/Evaluation", "14_Baseline_Baseline_plotInterarrivalTimesBinning")

#UTILIZATION
#nicht jeder worker einzeln da aehnlich
getMeanUtilizationSystem(1)
e15 <- plotUtilizationSystem() + i3_generateTheme()
i3_saveFig(e15, "C:/BA/Evaluation", "15_Baseline_Baseline_plotUtilizationSystem")

cut_Baseline_Gamma_w1 <- cutData(Baseline_Gamma_w1, 60)
cut_Baseline_Gamma_w2 <- cutData(Baseline_Gamma_w2, 60)
cut_Baseline_Gamma_w3 <- cutData(Baseline_Gamma_w3, 60)
cut_Baseline_Gamma_w4 <- cutData(Baseline_Gamma_w4, 60)
workerList <- list(cut_Baseline_Gamma_w1, cut_Baseline_Gamma_w2, cut_Baseline_Gamma_w3, cut_Baseline_Gamma_w4)

getMeanUtilizationSystem(1)
e16 <- plotUtilizationSystem() + i3_generateTheme()
i3_saveFig(e16, "C:/BA/Evaluation", "16_Baseline_Baseline_plotUtilizationSystemCut")

loadDataset("Baseline_Gamma", 4)
workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)

#OFFER
#getMeanOfferSystem(4, 1)
#plotOfferSystem(4)
#i3_saveFig(plotOfferSystem(4), "C:/BA/Evaluation", "19_Baseline_plotOfferSystem")

#cut_Baseline_Gamma_w1 <- cutData(Baseline_Gamma_w1, 60)
#cut_Baseline_Gamma_w2 <- cutData(Baseline_Gamma_w2, 60)
#cut_Baseline_Gamma_w3 <- cutData(Baseline_Gamma_w3, 60)
#cut_Baseline_Gamma_w4 <- cutData(Baseline_Gamma_w4, 60)
#workerList <- list(cut_Baseline_Gamma_w1, cut_Baseline_Gamma_w2, cut_Baseline_Gamma_w3, cut_Baseline_Gamma_w4)

#getMeanOfferSystem(4, 1)
#plotOfferSystem(4)
#i3_saveFig(plotOfferSystem(4), "C:/BA/Evaluation", "20_Baseline_plotOfferSystemCut")

#loadDataset("Baseline_Gamma", 4)
#workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)

#B R A K E   T H E   S Y S T E M

#MEAN WAITING TIMES
loadDataset("Baseline_Gamma", 4)
workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)
allWorkers <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95))
w1 <- getMeanWaitingTimeCurrentDataset(1)

loadDataset("Baseline_Gamma_5xLoad", 4)
workerList <- list(Baseline_Gamma_5xLoad_w1, Baseline_Gamma_5xLoad_w2, Baseline_Gamma_5xLoad_w3, Baseline_Gamma_5xLoad_w4)
allWorkers <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95))
w5 <- getMeanWaitingTimeCurrentDataset(5)

loadDataset("Baseline_Gamma_10xLoad", 4)
workerList <- list(Baseline_Gamma_10xLoad_w1, Baseline_Gamma_10xLoad_w2, Baseline_Gamma_10xLoad_w3, Baseline_Gamma_10xLoad_w4)
allWorkers <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95))
w10 <- getMeanWaitingTimeCurrentDataset(10)

loadDataset("Baseline_Gamma_15xLoad", 4)
workerList <- list(Baseline_Gamma_15xLoad_w1, Baseline_Gamma_15xLoad_w2, Baseline_Gamma_15xLoad_w3, Baseline_Gamma_15xLoad_w4)
allWorkers <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95))
w15 <- getMeanWaitingTimeCurrentDataset(15)

loadDataset("Baseline_Gamma_20xLoad", 4)
workerList <- list(Baseline_Gamma_20xLoad_w1, Baseline_Gamma_20xLoad_w2, Baseline_Gamma_20xLoad_w3, Baseline_Gamma_20xLoad_w4)
allWorkers <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95))
w20 <- getMeanWaitingTimeCurrentDataset(20)

loadDataset("Baseline_Gamma_25xLoad", 4)
workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)
allWorkers <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95))
w25 <- getMeanWaitingTimeCurrentDataset(25)

loadDataset("Baseline_Gamma_30xLoad", 4)
workerList <- list(Baseline_Gamma_30xLoad_w1, Baseline_Gamma_30xLoad_w2, Baseline_Gamma_30xLoad_w3, Baseline_Gamma_30xLoad_w4)
allWorkers <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95))
w30 <- getMeanWaitingTimeCurrentDataset(30)

loadDataset("Baseline_Gamma_35xLoad", 4)
workerList <- list(Baseline_Gamma_35xLoad_w1, Baseline_Gamma_35xLoad_w2, Baseline_Gamma_35xLoad_w3, Baseline_Gamma_35xLoad_w4)
allWorkers <- allWorkers %>% filter(waitingTime < quantile(waitingTime, 0.95))
w35 <- getMeanWaitingTimeCurrentDataset(35)

w <- rbind(w1, w5, w10, w15, w20, w25, w30, w35)
w <- rbind(w1, w5, w10, w15, w20, w25, w30)
w <- rbind(w1, w5, w10, w15, w20, w25)

f17 <- ggplot(data = w, aes(x = loadFactor, y = meanWaitingTime)) +
  geom_bar(stat = "identity", fill = cbPalette[8]) +
  geom_errorbar(aes(x=loadFactor, ymin=meanWaitingTime-ic, ymax=meanWaitingTime+ic), width = 0.4, size = 0.5) +
  xlab("Load factor") +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25),
                   labels=c(1, 5, 10, 15, 20, 25)) +
  ylab("Mean Waiting Time [microseconds]") +
  i3_generateTheme()

i3_saveFig(f17, "C:/BA/Evaluation", "f17_Baseline_Datasets_WaitingTime")

w <- rbind(w1, w5, w10, w15, w20, w25, w30)

f18 <- ggplot(data = w, aes(x = loadFactor, y = meanWaitingTime)) +
  geom_bar(stat = "identity", fill = cbPalette[8]) +
  geom_errorbar(aes(x=loadFactor, ymin=meanWaitingTime-ic, ymax=meanWaitingTime+ic), width = 0.4, size = 0.5) +
  xlab("Load factor") +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30),
                     labels=c(1, 5, 10, 15, 20, 25, 30)) +
  ylab("Mean Waiting Time [microseconds]") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  i3_generateTheme()

i3_saveFig(f18, "C:/BA/Evaluation", "f18__Datasets_WaitingTime")

#MEAN SOJOURN TIMES
#loadDataset("Baseline_Gamma", 4)
#workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)
#s1 <- getMeanSojournTimeCurrentDataset("Baseline_Gamma", 1)

#loadDataset("Baseline_Gamma_5xLoad", 4)
#workerList <- list(Baseline_Gamma_5xLoad_w1, Baseline_Gamma_5xLoad_w2, Baseline_Gamma_5xLoad_w3, Baseline_Gamma_5xLoad_w4)
#s5 <- getMeanSojournTimeCurrentDataset("Baseline_Gamma_5xLoad", 5)

#loadDataset("Baseline_Gamma_10xLoad", 4)
#workerList <- list(Baseline_Gamma_10xLoad_w1, Baseline_Gamma_10xLoad_w2, Baseline_Gamma_10xLoad_w3, Baseline_Gamma_10xLoad_w4)
#s10 <- getMeanSojournTimeCurrentDataset("Baseline_Gamma_10xLoad", 10)

#loadDataset("Baseline_Gamma_15xLoad", 4)
#workerList <- list(Baseline_Gamma_15xLoad_w1, Baseline_Gamma_15xLoad_w2, Baseline_Gamma_15xLoad_w3, Baseline_Gamma_15xLoad_w4)
#s15 <- getMeanSojournTimeCurrentDataset("Baseline_Gamma_15xLoad", 15)

#loadDataset("Baseline_Gamma_20xLoad", 4)
#workerList <- list(Baseline_Gamma_20xLoad_w1, Baseline_Gamma_20xLoad_w2, Baseline_Gamma_20xLoad_w3, Baseline_Gamma_20xLoad_w4)
#s20 <- getMeanSojournTimeCurrentDataset("Baseline_Gamma_20xLoad", 20)

#loadDataset("Baseline_Gamma_25xLoad", 4)
#workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)
#s25 <- getMeanSojournTimeCurrentDataset("Baseline_Gamma_25xLoad", 25)

#loadDataset("Baseline_Gamma_30xLoad", 4)
#workerList <- list(Baseline_Gamma_30xLoad_w1, Baseline_Gamma_30xLoad_w2, Baseline_Gamma_30xLoad_w3, Baseline_Gamma_30xLoad_w4)
#s30 <- getMeanSojournTimeCurrentDataset("Baseline_Gamma_30xLoad", 30)

#loadDataset("Baseline_Gamma_35xLoad", 4)
#workerList <- list(Baseline_Gamma_35xLoad_w1, Baseline_Gamma_35xLoad_w2, Baseline_Gamma_35xLoad_w3, Baseline_Gamma_35xLoad_w4)
#s35 <- getMeanSojournTimeCurrentDataset("Baseline_Gamma_35xLoad", 35)

#s <- rbind(s1, s5, s10, s15, s20, s25, s30, s35)
#s <- rbind(s1, s5, s10, s15, s20, s25, s30)
#s <- rbind(s1, s5, s10, s15, s20, s25)

#sPlot <- ggplot(data = s, aes(x = loadFactor, y = meanSojournTime)) +
#  geom_bar(stat = "identity", fill = cbPalette[8]) +
#  geom_errorbar(aes(x=loadFactor, ymin=meanSojournTime-ic, ymax=meanSojournTime+ic), width = 0.4, size = 0.5) +
#  xlab("Load Factor") +
#  ylab("Mean Sojourn Time [microseconds]")
#i3_saveFig(sPlot, "C:/BA/Evaluation", "23_Datasets_SojournTimes")

#s <- rbind(s1, s5, s10, s15, s20, s25, s30)

#sPlot <- ggplot(data = s, aes(x = loadFactor, y = meanSojournTime)) +
#  geom_bar(stat = "identity", fill = cbPalette[8]) +
#  geom_errorbar(aes(x=loadFactor, ymin=meanSojournTime-ic, ymax=meanSojournTime+ic), width = 0.4, size = 0.5) +
#  xlab("Load Factor") +
#  ylab("Mean Sojourn Time [microseconds]")
#i3_saveFig(sPlot, "C:/BA/Evaluation", "24_Datasets_SojournTimes2")

#PCT FAILED MESSAGES
loadDataset("Baseline_Gamma", 4)
workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)
p1 <- getPctFailedCurrentDataset("Baseline_Gamma", 10000, 1)

loadDataset("Baseline_Gamma_5xLoad", 4)
workerList <- list(Baseline_Gamma_5xLoad_w1, Baseline_Gamma_5xLoad_w2, Baseline_Gamma_5xLoad_w3, Baseline_Gamma_5xLoad_w4)
p5 <- getPctFailedCurrentDataset("Baseline_Gamma_5xLoad", 10000, 5)

loadDataset("Baseline_Gamma_10xLoad", 4)
workerList <- list(Baseline_Gamma_10xLoad_w1, Baseline_Gamma_10xLoad_w2, Baseline_Gamma_10xLoad_w3, Baseline_Gamma_10xLoad_w4)
p10 <- getPctFailedCurrentDataset("Baseline_Gamma_10xLoad", 10000, 10)

loadDataset("Baseline_Gamma_15xLoad", 4)
workerList <- list(Baseline_Gamma_15xLoad_w1, Baseline_Gamma_15xLoad_w2, Baseline_Gamma_15xLoad_w3, Baseline_Gamma_15xLoad_w4)
p15 <- getPctFailedCurrentDataset("Baseline_Gamma_15xLoad", 10000, 15)

loadDataset("Baseline_Gamma_20xLoad", 4)
workerList <- list(Baseline_Gamma_20xLoad_w1, Baseline_Gamma_20xLoad_w2, Baseline_Gamma_20xLoad_w3, Baseline_Gamma_20xLoad_w4)
p20 <- getPctFailedCurrentDataset("Baseline_Gamma_20xLoad", 10000, 20)

loadDataset("Baseline_Gamma_25xLoad", 4)
workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)
p25 <- getPctFailedCurrentDataset("Baseline_Gamma_25xLoad", 10000, 25)

loadDataset("Baseline_Gamma_30xLoad", 4)
workerList <- list(Baseline_Gamma_30xLoad_w1, Baseline_Gamma_30xLoad_w2, Baseline_Gamma_30xLoad_w3, Baseline_Gamma_30xLoad_w4)
p30 <- getPctFailedCurrentDataset("Baseline_Gamma_30xLoad", 10000, 30)

loadDataset("Baseline_Gamma_35xLoad", 4)
workerList <- list(Baseline_Gamma_35xLoad_w1, Baseline_Gamma_35xLoad_w2, Baseline_Gamma_35xLoad_w3, Baseline_Gamma_35xLoad_w4)
p35 <- getPctFailedCurrentDataset("Baseline_Gamma_35xLoad", 10000, 35)

p <- rbind(p1, p5, p10, p15, p20, p25, p30, p35)
p <- rbind(p1, p5, p10, p15, p20, p25, p30)
p <- rbind(p1, p5, p10, p15, p20, p25)

#pPlot <- ggplot(data = p, aes(x = loadFactor, y = pct * 100)) +
#  geom_bar(stat = "identity", fill = cbPalette[8]) +
#  xlab("Load Factor") +
#  ylab("Percentage of failed messages")
#i3_saveFig(pPlot, "C:/BA/Evaluation", "25_Datasets_PctFailed")

#p <- rbind(p1, p5, p10, p15, p20, p25, p30)

#pPlot <- ggplot(data = p, aes(x = loadFactor, y = pct * 100)) +
#  geom_bar(stat = "identity", fill = cbPalette[8]) +
#  xlab("Load Factor") +
#  ylab("Percentage of failed messages")
#i3_saveFig(pPlot, "C:/BA/Evaluation", "26_Datasets_PctFailed2")

p <- rbind(p1, p5, p10, p15, p20, p25, p30, p35)

f19 <- ggplot(data = p, aes(x = loadFactor, y = pct * 100)) +
  geom_bar(stat = "identity", fill = cbPalette[8]) +
  xlab("Load factor") +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30, 35),
                     labels=c(1, 5, 10, 15, 20, 25, 30, 35)) +
  ylab("Percentage of failed messages") +
  i3_generateTheme()
i3_saveFig(f19, "C:/BA/Evaluation", "f19_Datasets_PctFailed3")

#UTILIZATION
loadDataset("Baseline_Gamma", 4)
workerList <- list(Baseline_Gamma_w1, Baseline_Gamma_w2, Baseline_Gamma_w3, Baseline_Gamma_w4)
u1 <- getMeanUtilizationSystem(1)

loadDataset("Baseline_Gamma_5xLoad", 4)
workerList <- list(Baseline_Gamma_5xLoad_w1, Baseline_Gamma_5xLoad_w2, Baseline_Gamma_5xLoad_w3, Baseline_Gamma_5xLoad_w4)
u5 <- getMeanUtilizationSystem(5)

loadDataset("Baseline_Gamma_10xLoad", 4)
workerList <- list(Baseline_Gamma_10xLoad_w1, Baseline_Gamma_10xLoad_w2, Baseline_Gamma_10xLoad_w3, Baseline_Gamma_10xLoad_w4)
u10 <- getMeanUtilizationSystem(10)

loadDataset("Baseline_Gamma_15xLoad", 4)
workerList <- list(Baseline_Gamma_15xLoad_w1, Baseline_Gamma_15xLoad_w2, Baseline_Gamma_15xLoad_w3, Baseline_Gamma_15xLoad_w4)
u15 <- getMeanUtilizationSystem(15)

loadDataset("Baseline_Gamma_20xLoad", 4)
workerList <- list(Baseline_Gamma_20xLoad_w1, Baseline_Gamma_20xLoad_w2, Baseline_Gamma_20xLoad_w3, Baseline_Gamma_20xLoad_w4)
u20 <- getMeanUtilizationSystem(20)

loadDataset("Baseline_Gamma_25xLoad", 4)
workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)
u25 <- getMeanUtilizationSystem(25)

loadDataset("Baseline_Gamma_30xLoad", 4)
workerList <- list(Baseline_Gamma_30xLoad_w1, Baseline_Gamma_30xLoad_w2, Baseline_Gamma_30xLoad_w3, Baseline_Gamma_30xLoad_w4)
u30 <- getMeanUtilizationSystem(30)

loadDataset("Baseline_Gamma_35xLoad", 4)
workerList <- list(Baseline_Gamma_35xLoad_w1, Baseline_Gamma_35xLoad_w2, Baseline_Gamma_35xLoad_w3, Baseline_Gamma_35xLoad_w4)
u35 <- getMeanUtilizationSystem(35)

u <- rbind(u1, u5, u10, u15, u20, u25, u30, u35)
u <- rbind(u1, u5, u10, u15, u20, u25, u30)

f20 <- ggplot(data = u, aes(x = loadFactor, y = meanSystemUtilization)) +
  geom_bar(stat = "identity", fill = cbPalette[8]) +
  xlab("Load factor") +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30, 35),
                     labels=c(1, 5, 10, 15, 20, 25, 30, 35)) +
  ylab("Percentage of Utilization") +
  i3_generateTheme()

i3_saveFig(f20, "C:/BA/Evaluation", "f20_Datasets_Utilization")

#last row NaN, row ist removed from plot
#u <- rbind(u1, u5, u10, u15, u20, u25, u30, u35)

#uPlot <- ggplot(data = u, aes(x = loadFactor, y = meanSystemUtilization)) +
#  geom_bar(stat = "identity", fill = cbPalette[8]) +
#  xlab("Load Factor") +
#  ylab("Percentage of Utilization")
#uPlot

#ANALYZATION WHY DID SYSTEM BREAK?
#25xLoad peak, 30xLoad kaputt, vorschlag: sojournTime + utilization

#25xLoad
loadDataset("Baseline_Gamma_25xLoad", 4)
workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)

#sojournTime
#e21
getStatisticsSojournTime("Baseline_Gamma_25xLoad", allWorkers, 25)
e21 <- plotMeanSojournTimes("Baseline_Gamma_25xLoad", allWorkers) + i3_generateTheme()
i3_saveFig(e21, "C:/BA/Evaluation", "21_25xLoad_meanSojournTimes")

#f21
getStatisticsSojournTime("Baseline_Gamma_25xLoad", allWorkers, 25)
f21 <- plotMeanSojournTimesFiltered("Baseline_Gamma_25xLoad", allWorkers) + i3_generateTheme()
i3_saveFig(f21, "C:/BA/Evaluation", "f21_25xLoad_meanSojournTimes")

#f21+
plotCdfWaitingTimesColors(allWorkers) + i3_generateTheme()
i3_saveFig(last_plot(), "C:/BA/Evaluation", "f21+_ecdf_25load")

#e22
loadDataset("Baseline_Gamma_25xLoad", 4)
workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)

plotSojournTimes("Baseline_Gamma_25xLoad", allWorkers) + i3_generateTheme(margin.left = 20, margin.right = 20)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "22_25xLoad_plotSojournTimes")

#f22
loadDataset("Baseline_Gamma_25xLoad", 4)
workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)

plotSojournTimesFiltered("Baseline_Gamma_25xLoad", allWorkers) + i3_generateTheme(margin.left = 20, margin.right = 20)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "f22_25xLoad_plotSojournTimesFiltered")

#e23
plotSojournTimesFacetGrid("Baseline_Gamma_25xLoad") + theme(strip.text.y = element_text(size = 15)) + i3_generateTheme(fontSize = 15, margin.left = 20, margin.right = 20)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "23_25xLoad_plotSojournTimesFacetGrid")
#begin wiederholt (pieks)
loadDataset("Baseline_Gamma_25xLoad2", 4)
workerList <- list(Baseline_Gamma_25xLoad2_w1, Baseline_Gamma_25xLoad2_w2, Baseline_Gamma_25xLoad2_w3, Baseline_Gamma_25xLoad2_w4)
#kein i3save moeglich, pdf weiss
#auch kein R speichern moeglich
#e24
plotSojournTimes("Baseline_Gamma_25xLoad2", allWorkers) + i3_generateTheme(margin.left = 20, margin.right = 20)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "24_25xLoad2_plotSojournTimes")

#neu
plotSojournTimesFiltered <- function(datasetAsString, worker){
  data <- worker %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(sojournTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime,
           SupervisorSendTime = SupervisorSendTime - min(SupervisorSendTime))

  data <- filter(data, sojournTime <= 2e+05)

  ggplot(data = data %>% sample_n(size = nrow(data) * 0.1), mapping = aes(x = (SupervisorSendTime / 1000000), y = sojournTime)) +
    geom_line() +
    xlab("Time [seconds]") +
    ylab("Soujourn Time [microseconds]")
}

loadDataset("Baseline_Gamma_25xLoad", 4)
workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)

plotSojournTimes("Baseline_Gamma_25xLoad", allWorkers) + i3_generateTheme(margin.left = 20, margin.right = 20)

teste24 <- plotSojournTimesFiltered("Baseline_Gamma_25xLoad", allWorkers) + i3_generateTheme(margin.left = 20, margin.right = 20)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "test_e24")

#e25
plotSojournTimesFacetGrid("Baseline_Gamma_25xLoad2") + theme(strip.text.y = element_text(size = 15)) +i3_generateTheme(fontSize = 15, margin.left = 20, margin.right = 20)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "25_25xLoad2_plotSojournTimesFacetGrid")

#neu
plotSojournTimesFacetGridFiltered <- function(datasetAsString){
  data <- allWorkers %>%
    left_join(get(paste0(datasetAsString, "_supervisor")) %>% select(sup_messageArriveTime = messageArriveTime, id), by = "id") %>%
    mutate(sojournTime = messageArriveTime + waitingTime + processingTime - sup_messageArriveTime,
           SupervisorSendTime = SupervisorSendTime - min(SupervisorSendTime))

  data <- filter(data, sojournTime <= 2e+05)

  ggplot(data = data %>% sample_n(size = nrow(data) * 0.1), mapping = aes(x = (SupervisorSendTime / 1000000), y = sojournTime)) +
    geom_line() +
    facet_grid(rows = vars(workerId)) +
    xlab("Time [seconds]") +
    #ylim(0, 9000) +
    ylab("Soujourn Time [microseconds]")
}

loadDataset("Baseline_Gamma_25xLoad", 4)
workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)

teste25 <- plotSojournTimesFacetGridFiltered("Baseline_Gamma_25xLoad") + theme(strip.text.y = element_text(size = 15)) +i3_generateTheme(fontSize = 15, margin.left = 20, margin.right = 20)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "test_e25")

#end wiederholt (pieks)

loadDataset("Baseline_Gamma_25xLoad", 4)
workerList <- list(Baseline_Gamma_25xLoad_w1, Baseline_Gamma_25xLoad_w2, Baseline_Gamma_25xLoad_w3, Baseline_Gamma_25xLoad_w4)
#e26
plotSojournTimesBinningFacetGrid("Baseline_Gamma_25xLoad", allWorkers) + theme(strip.text.y = element_text(size = 15)) + i3_generateTheme()
i3_saveFig(last_plot(), "C:/BA/Evaluation", "26_25xLoad_plotSojournTimesBinningFacetGrid")

#utilization
#25xLoad
e27 <- plotUtilizationSystem() + i3_generateTheme()
i3_saveFig(e27, "C:/BA/Evaluation", "27_25xLoad_plotUtilizationSystem")

#(zeitreihe fuer waiting time noetig? --> nein, sojournTime schliesst das mit ein und reich aus)

#30xLoad
loadDataset("Baseline_Gamma_30xLoad", 4)
workerList <- list(Baseline_Gamma_30xLoad_w1, Baseline_Gamma_30xLoad_w2, Baseline_Gamma_30xLoad_w3, Baseline_Gamma_30xLoad_w4)

#sojournTime
getStatisticsSojournTime("Baseline_Gamma_30xLoad", allWorkers, 30)
f28 <- plotMeanSojournTimesFiltered("Baseline_Gamma_30xLoad", allWorkers) + i3_generateTheme()
i3_saveFig(f28, "C:/BA/Evaluation", "f28_30xLoad_plotMeanSojournTimes")
#kein i3save moeglich, pdf weiss
#auch kein R speichern moeglich
#e29
plotSojournTimes("Baseline_Gamma_30xLoad", allWorkers) + i3_generateTheme(margin.left = 20, margin.right = 20)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "29_30xLoad_plotSojournTimes")
#e30
plotSojournTimesFacetGrid("Baseline_Gamma_30xLoad") + theme(strip.text.y = element_text(size = 15)) +i3_generateTheme(fontSize = 15, margin.left = 20, margin.right = 20)
i3_saveFig(last_plot(), "C:/BA/Evaluation", "30_30xLoad_plotSojournTimesFacetGrid")
#erroneous
#plotSojournTimesBinningFacetGrid("Baseline_Gamma_30xLoad", allWorkers)

#utilization
#e31
plotUtilizationSystem() + i3_generateTheme()
i3_saveFig(last_plot(), "C:/BA/Evaluation", "31_30xLoad_plotUtilizationSystem")

#neu
#30xLoad
loadDataset("Baseline_Gamma_30xLoad", 4)
workerList <- list(Baseline_Gamma_30xLoad_w1, Baseline_Gamma_30xLoad_w2, Baseline_Gamma_30xLoad_w3, Baseline_Gamma_30xLoad_w4)

plotUtilizationSystemFiltered <- function(){
  data1 <- filter(getUtilizationSystem(), bin <= 60000000)

  data2 <- filter(getUtilizationSystem(), bin > 60000000)

  data2 <- filter(data2, utilization <= 1.0,
                  utilization > 0.98)

  data <- bind_rows(data1, data2)

  ggplot(data = data, mapping = aes(x = bin / 1000000, y = utilization)) +
    geom_line() +
    xlab("Time [seconds]") +
    xlim(0, 1000) +
    ylab("Percentage of Utilization") +
    ylim(0, 1)
}

plotUtilizationSystemFiltered() + i3_generateTheme()
i3_saveFig(last_plot(), "C:/BA/Evaluation", "test_e31")

#outlier analysis
loadDataset("Baseline_Gamma_30xLoad", 4)
workerList <- list(Baseline_Gamma_30xLoad_w1, Baseline_Gamma_30xLoad_w2, Baseline_Gamma_30xLoad_w3, Baseline_Gamma_30xLoad_w4)

test2 <- getUtilizationSingleWorker(Baseline_Gamma_30xLoad_w1)

slice_max(test2, utilization, n = 20)

#max peak
cut <- Baseline_Gamma_30xLoad_w1 %>%
  summarize(messageArriveTime = (messageArriveTime - min(messageArriveTime)) / 1000000,
            processingTime = processingTime)

cut <- filter(cut, messageArriveTime >= 571)
cut <- filter(cut, messageArriveTime < 572)

tail(cut, 10)

getUtilizationSingleWorker(cut)

cut <- slice_max(cut, processingTime, n = 848)

cut <- cut[-1,]

getUtilizationSingleWorker(cut)

#second max peak


#E N D   E V A L U A T I O N

#example functions
i3_saveFig(plotMeanWaitingTimes(allWorkers), "C:/BA/test_figures", "M70_w1_MeanWaitingTimes")
getStatisticsWaitingTime(Baseline_Gamma_w1, 1)
getStatisticsProcessingTime(Baseline_Gamma_w1, 1)
getStatisticsSojournTime("Baseline_Gamma", Baseline_Gamma_w1, 1)
getStatisticsInterarrivalTimes(allWorkers, 1)
getStatisticsPctFailedMessages("Baseline_Gamma", allWorkers, 3000, 1)
plotMeanWaitingTimes(Baseline_Gamma_w1)
plotMeanProcessingTimes(allWorkers)
plotMeanSojournTimes("Baseline_Gamma", allWorkers)
plotPctFailedMessages("Baseline_Gamma", Baseline_Gamma_w1, 1000, "1")
plotCdfWaitingTimes(allWorkers)
plotPdfWaitingTimes(allWorkers)
plotCdfProcessingTimes(allWorkers)
plotPdfProcessingTimes(allWorkers)
getUtilizationSingleWorker(Baseline_Gamma_w1)
getMeanUtilizationSingleWorker(Baseline_Gamma_w1)
plotUtilizationSingleWorker(Baseline_Gamma_w1)
getUtilizationSystem()
getMeanUtilizationSystem(1)
plotUtilizationSystem()
getOfferSingleWorker(Baseline_Gamma_w1)
getMeanOfferSingleWorker(Baseline_Gamma_w1)
plotOfferSingleWorker(Baseline_Gamma_w1)
getOfferSystem(4)
getMeanOfferSystem(4, 1)
plotOfferSystem(4)
setWdToDataset("Baseline_Gamma")
cut_Baseline_Gamma_w1 <- cutData(Baseline_Gamma_w1, 600)
#neu
plotSojournTimes("Baseline_Gamma", cut_Baseline_Gamma_w1)
plotSojournTimesXlim("Baseline_Gamma", Baseline_Gamma_w1, 10)
plotSojournTimesFacetGrid("Baseline_Gamma")
plotSojournTimesBinningFacetGrid("Baseline_Gamma", allWorkers)
plotInterarrivalTimesBinning(allWorkers)
plotInterarrivalTimes(Baseline_Gamma_w1)
getMeanWaitingTimeCurrentDataset(1)
getMeanProcessingTimeCurrentDataset(1)
getMeanSojournTimeCurrentDataset("Baseline_Gamma", 1)
getMeanIatCurrentDataset(1)
getPctFailedCurrentDataset("Baseline_Gamma", 3000, 1)