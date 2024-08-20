package com.proyect.masterdata.services.impl;

import com.itextpdf.kernel.colors.ColorConstants;
import com.itextpdf.kernel.geom.PageSize;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.kernel.pdf.canvas.draw.ILineDrawer;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.Style;
import com.itextpdf.layout.element.LineSeparator;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.properties.TextAlignment;
import com.itextpdf.layout.properties.UnitValue;
import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IPdfGenerator;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class PdfGeneratorImpl implements IPdfGenerator {
    private ILineDrawer iLineDrawer;
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductPriceRepository productPriceRepository;
    @Override
    public CompletableFuture<InputStream> generateOrderReport(Long orderId,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                ordering = orderingRepository.findByClientIdAndId(user.getClientId(),orderId);
            }
            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }
            try{
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                PdfWriter writer = new PdfWriter(out);
                PdfDocument pdfDoc = new PdfDocument(writer);
                pdfDoc.setDefaultPageSize(PageSize.A7);

                Document document = new Document(pdfDoc);
                document.setMargins(2,2,2,2);

                Style headerStyle = new Style().setFontSize(8).setBold().setFontColor(ColorConstants.BLACK);
                Style normalStyle = new Style().setFontSize(7).setFontColor(ColorConstants.BLACK);

                // Separator

                LineSeparator separator = new LineSeparator(iLineDrawer);
                separator.setMarginTop(2).setMarginBottom(2);

                // Header Section
                document.add(new Paragraph("#"+ordering.getId()+" "+ordering.getCustomer().getName().toUpperCase()).addStyle(headerStyle).setMargin(0).setPadding(0));
                document.add(new Paragraph(ordering.getDeliveryAddress()).addStyle(normalStyle).setMargin(0).setPadding(0));
                document.add(new Paragraph(ordering.getCustomer().getDistrict().getName()).addStyle(normalStyle).setMargin(0).setPadding(0));
                document.add(new Paragraph(ordering.getCustomer().getDistrict().getProvince().getName()).addStyle(normalStyle).setMargin(0).setPadding(0));
                document.add(new Paragraph(ordering.getCustomer().getDistrict().getProvince().getDepartment().getName()).addStyle(normalStyle).setMargin(0).setPadding(0));
                document.add(new Paragraph(ordering.getCustomer().getPhone()).addStyle(normalStyle).setMargin(0).setPadding(0));

                // Add a separator
                document.add(separator);

                document.add(new Paragraph(ordering.getSaleChannel().getName()).addStyle(normalStyle).setMargin(0).setPadding(0).setTextAlignment(TextAlignment.CENTER));
                document.add(new Paragraph(ordering.getManagementType().getName()).addStyle(normalStyle).setMargin(0).setPadding(0).setTextAlignment(TextAlignment.CENTER));

                // Add a separator
                document.add(separator);
                // Table for item info
                float[] columnWidths = {1,4}; // Two Columns with proportional widths
                Table table = new Table(UnitValue.createPercentArray(columnWidths)).useAllAvailableWidth();
                table.setMargin(0).setPadding(0);

                List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(ordering.getId());
                int index = 1;
                double saleAmount = 0.00;
                for(OrderItem orderItem:orderItems){
                    table.addCell(new Paragraph(index+".").addStyle(normalStyle).setMargin(0).setPadding(0));
                    table.addCell(new Paragraph(
                            orderItem.getProduct().getCategoryProduct().getName() + " - "
                                    + orderItem.getProduct().getModel().getBrand().getName() + " - "
                                    + orderItem.getProduct().getModel().getName() + " - "
                                    + orderItem.getProduct().getColor().getName() + " - "
                                    + orderItem.getProduct().getSize().getName() + " x "
                                    + orderItem.getQuantity() + " " + orderItem.getProduct().getUnit().getName()).addStyle(normalStyle).setMargin(0).setPadding(0));
                    ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                    if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                        saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * orderItem.getQuantity()) * (orderItem.getDiscountAmount() / 100));
                    }
                    if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                        saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - orderItem.getDiscountAmount();
                    }
                    if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                        saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                    }
                    index++;
                }

                document.add(table);

                double totalDuePayment=0;
                if(Objects.equals(ordering.getDiscount().getName(), "PORCENTAJE")){
                    totalDuePayment = (saleAmount-((saleAmount)*(ordering.getDiscountAmount()/100))+ordering.getDeliveryAmount())-ordering.getAdvancedPayment();
                }
                if(Objects.equals(ordering.getDiscount().getName(), "MONTO")){
                    totalDuePayment = (saleAmount-ordering.getDiscountAmount()+ordering.getDeliveryAmount())-ordering.getAdvancedPayment();
                }
                if(Objects.equals(ordering.getDiscount().getName(), "NO APLICA")){
                    totalDuePayment = (saleAmount+ordering.getDeliveryAmount())-ordering.getAdvancedPayment();
                }

                // Add a separator
                document.add(separator);

                // Total Section
                document.add(new Paragraph("Total: $"+ BigDecimal.valueOf(saleAmount+ordering.getDeliveryAmount()).setScale(2, RoundingMode.HALF_EVEN)).addStyle(headerStyle).setMargin(0).setPadding(0));
                document.add(new Paragraph("Costo producto: $"+BigDecimal.valueOf(saleAmount).setScale(2, RoundingMode.HALF_EVEN)).addStyle(normalStyle).setMargin(0).setPadding(0));
                document.add(new Paragraph("Costo envio: $"+BigDecimal.valueOf(ordering.getDeliveryAmount()).setScale(2, RoundingMode.HALF_EVEN)).addStyle(normalStyle).setMargin(0).setPadding(0));
                document.add(new Paragraph("Pendiente por pagar: $"+BigDecimal.valueOf(totalDuePayment).setScale(2, RoundingMode.HALF_EVEN)).addStyle(headerStyle).setMargin(0).setPadding(0));

                // Add a separator
                document.add(separator);

                // Footer Section
                document.add(new Paragraph("RUC 20609605601").addStyle(normalStyle).setMargin(0).setPadding(0).setTextAlignment(TextAlignment.CENTER));
                document.add(new Paragraph("Corporación ARANNI SAC").addStyle(normalStyle).setMargin(0).setPadding(0).setTextAlignment(TextAlignment.CENTER));
                document.add(new Paragraph("Lima - Perú").addStyle(normalStyle).setMargin(0).setPadding(0).setTextAlignment(TextAlignment.CENTER));
                document.add(new Paragraph("Atención al cliente: 970334874").addStyle(normalStyle).setMargin(0).setPadding(0).setTextAlignment(TextAlignment.CENTER));
                LocalDateTime now = LocalDateTime.now();
                DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yy");
                String formattedDate = now.format(dateFormatter);
                DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss");
                String formattedTime = now.format(timeFormatter);
                // Date and Time
                document.add(new Paragraph("Impreso en: "+formattedDate+" - "+formattedTime).addStyle(normalStyle).setMargin(0).setPadding(0).setTextAlignment(TextAlignment.CENTER));

                document.close();

                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
