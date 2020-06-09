package BA

case class Request(id: Long, messageType: String)

case class Response(id: Long, messageType: String)

case class SupervisorToWorker(id: Long, supervisorSendTime: Long, messageType: String)

case class writeToFileRequest()

case class writeToFileResponse()
