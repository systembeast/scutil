package com.sysbeast.scutil

import java.security.cert.X509Certificate

import javax.net.ssl._

object SslUtil {
  val DO_NOT_VERIFY = new HostnameVerifier() {
    override def verify(s: String, sslSession: SSLSession): Boolean = true
  }

  val trustAllCerts = Array[TrustManager] {
    new X509TrustManager() {
      override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}
      override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}
      override def getAcceptedIssuers = Array.empty[X509Certificate]
    }
  }
  def trustAllHosts(connection: HttpsURLConnection): SSLSocketFactory = {
    val oldFactory = connection.getSSLSocketFactory();
    val sc = SSLContext.getInstance("TLS")
    sc.init(null, trustAllCerts, new java.security.SecureRandom())
    val newFactory = sc.getSocketFactory()
    connection.setSSLSocketFactory(newFactory)
    oldFactory
  }


}
