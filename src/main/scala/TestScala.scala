import java.io.{BufferedWriter, FileWriter}
import scala.io.Source


object TestScala extends App {

  val invoice = Source
    .fromFile("src/main/resources/invoices.csv")
    .getLines
    .drop(1)
    .toList
    .map(e => e.split(","))

  val invoiceProcessed = invoice
    .map(e => {
      val period = e(0).replaceAll("-", "").substring(0, 6)
      val innSeller = e(1)
      val innBuyer = e(2)
      val ndsSeller = e(3)
      Invoice(period, innSeller.toInt, innBuyer.toInt, ndsSeller.toInt)
    })

  val groupedInvoicesSales = invoiceProcessed
    .groupBy(e => (e.period, e.innSeller))
    .mapValues(e => GroupedInvoiceSales(e.head.period, e.head.innSeller, e.map(_.ndsSeller).sum))

  val groupedInvoicesBoughts = invoiceProcessed
    .groupBy(e => (e.period, e.innBuyer))
    .mapValues(e => GroupedInvoiceBoughts(e.head.period, e.head.innBuyer, e.map(_.ndsSeller).sum))

  val vatReport = Source
    .fromFile("src/main/resources/vat_report.csv")
    .getLines
    .drop(1)
    .toList
    .map(e => e.split(","))

  val vatProcessed = vatReport
    .map(e => {
      val vatPeriod = e(1)
      val vatInn = e(0)
      val vatSeller = e(2)
      val vatOffset = e(3)
      (vatPeriod, vatInn.toInt) -> VatReport(vatPeriod, vatInn.toInt, vatSeller.toInt, vatOffset.toInt)
    }).toMap

  val outDiscrSales = groupedInvoicesSales
    .map(e => {
      if (vatProcessed.contains(e._1)) {
        Discrepency(
          e._2.period,
          e._2.innSeller,
          if (e._2.ndsSeller > vatProcessed(e._1).vatSeller) {
            e._2.ndsSeller - vatProcessed(e._1).vatSeller
          } else {
            0
          },
          0)
      } else {
        Discrepency(e._2.period, e._2.innSeller, e._2.ndsSeller, 0)
      }
    })
    .map(e => {
      (e.Period, e.Inn) -> e
    })
    .toMap

  val outDiscrOffset = vatProcessed
    .map(e => {
      if (groupedInvoicesBoughts.contains(e._1)) {
        outDiscrSales(e._1).OffsetDifference =
          if (e._2.vatOffset > groupedInvoicesBoughts(e._1).ndsBuyer) {
            e._2.vatOffset - groupedInvoicesBoughts(e._1).ndsBuyer
          } else {
            0
          }
        outDiscrSales(e._1)
      } else {
        outDiscrSales(e._1).OffsetDifference = e._2.vatOffset
        outDiscrSales(e._1)
      }
    })
    .map(e => {
      (e.Period, e.Inn) -> e
    })
    .toMap

  val expect = (outDiscrSales ++ outDiscrOffset)
    .valuesIterator
    .toList
    .filter(d => d.VatDifference != 0 || d.OffsetDifference != 0)
    .sortBy(e => (e.Period, e.Inn))


  implicit class CSVWrapper(val prod: Product) extends AnyVal {
    def toCSV() = prod.productIterator.map {
      case Some(value) => value
      case None => ""
      case rest => rest
    }.mkString(",")
  }

  val headerFields = classOf[Discrepency].getDeclaredFields.map(_.getName).toList.mkString(",")
  val csvExpect = expect.map(_.toCSV())

  val file = "src/main/resources/results.csv"

  val writer = new BufferedWriter(new FileWriter(file))
  writer.write(headerFields)
  csvExpect.foreach(p => {
    writer.write("\n")
    writer.write(p)
  }
  )
  writer.close()

}
