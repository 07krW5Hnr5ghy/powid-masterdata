package com.proyect.masterdata.services.impl;

import com.itextpdf.kernel.colors.ColorConstants;
import com.itextpdf.kernel.geom.PageSize;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.Style;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.properties.UnitValue;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IPdfGenerator;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class PdfGeneratorImpl implements IPdfGenerator {
    @Override
    public CompletableFuture<InputStream> generate() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            PdfWriter writer = new PdfWriter(out);
            PdfDocument pdfDoc = new PdfDocument(writer);
            pdfDoc.setDefaultPageSize(PageSize.A7);

            Document document = new Document(pdfDoc);
            document.setMargins(2,2,2,2);

            Style headerStyle = new Style().setFontSize(10).setBold().setFontColor(ColorConstants.BLACK);
            Style normalStyle = new Style().setFontSize(8).setFontColor(ColorConstants.BLACK);

            // Header Section
            document.add(new Paragraph("#15628 ALONSO FUENTES").addStyle(headerStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("CRA 123").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("CHACCHO").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("ANTONIO RAYMONDI").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("ANCASH").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("123456789").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("FALABELLA").addStyle(normalStyle).setMargin(0).setPadding(0));
            // Table for item info
            float[] columnWidths = {1,4}; // Two Columns with proportional widths
            Table table = new Table(UnitValue.createPercentArray(columnWidths)).useAllAvailableWidth();
            table.setMargin(0).setPadding(0);

            table.addCell(new Paragraph("1.").addStyle(normalStyle).setMargin(0).setPadding(0));
            table.addCell(new Paragraph("BOTINES - XIMO - R4 - ROJO - 35 x 1 und").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(table);

            // Total Section
            document.add(new Paragraph("Total: $1.00").addStyle(headerStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("Costo producto: $1.00").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("Costo envio: $0.00").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("Pendiente por pagar: $1.00").addStyle(headerStyle).setMargin(0).setPadding(0));

            // Footer Section
            document.add(new Paragraph("RUC 20609605601").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("Corporación ARANNI SAC").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("Lima - Perú").addStyle(normalStyle).setMargin(0).setPadding(0));
            document.add(new Paragraph("Atención al cliente: 970334874").addStyle(normalStyle).setMargin(0).setPadding(0));

            // Date and Time
            document.add(new Paragraph("Impreso en: 12/08/2024 - 21:26:17").addStyle(normalStyle).setMargin(0).setPadding(0));

            document.close();

            return new ByteArrayInputStream(out.toByteArray());
        });
    }
}
