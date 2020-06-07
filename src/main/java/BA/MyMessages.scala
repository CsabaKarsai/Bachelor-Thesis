package BA

case class Request(id: Long, requestType: String)

case class Response(id: Long, requestType: String)

case class SupervisorToWorker(id: Long, timestamp: Long, requestType: String)

case class writeToFileRequest()

case class writeToFileResponse()
