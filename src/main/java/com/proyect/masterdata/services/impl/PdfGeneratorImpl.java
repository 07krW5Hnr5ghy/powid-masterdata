package com.proyect.masterdata.services.impl;

import com.itextpdf.io.font.constants.StandardFonts;
import com.itextpdf.kernel.colors.ColorConstants;
import com.itextpdf.kernel.colors.DeviceGray;
import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.font.PdfFontFactory;
import com.itextpdf.kernel.geom.PageSize;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.kernel.pdf.canvas.draw.ILineDrawer;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.Style;
import com.itextpdf.layout.borders.Border;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.LineSeparator;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.properties.TextAlignment;
import com.itextpdf.layout.properties.UnitValue;
import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.DeliveryManifestDTO;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IPdfGenerator;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
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
    private final DeliveryManifestRepository deliveryManifestRepository;
    private final DeliveryManifestItemRepository deliveryManifestItemRepository;
    private final IUtil iUtil;
    @Override
    public CompletableFuture<InputStream> generateOrderReport(UUID orderId, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
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
                document.add(new Paragraph(ordering.getCustomer().getAddress().toUpperCase()).addStyle(normalStyle).setMargin(0).setPadding(0));
                document.add(new Paragraph(ordering.getCustomer().getReference().toUpperCase()).addStyle(normalStyle).setMargin(0).setPadding(0));
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
                            orderItem.getProduct().getSubCategoryProduct().getCategoryProduct().getName() + " - "
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

    @Override
    public CompletableFuture<InputStream> generateDeliveryManifestReport(String deliveryManifestId, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryManifest deliveryManifest;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
//                deliveryManifest = deliveryManifestRepository.findById(deliveryManifestId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
//            if(deliveryManifest==null){
//                throw new BadRequestExceptions(Constants.ErrorDeliveryStatusExists);
//            }
            try{
//                List<Ordering> orders = new ArrayList<>();
//                Set<Long> uniqueOrderNumbers = new HashSet<>();
//                List<DeliveryManifestItemDTO> deliveryManifestItemDTOS = deliveryManifestItemRepository.findAllById(deliveryManifest.getId())
//                        .stream().map(deliveryManifestItem -> {
//                            if(!uniqueOrderNumbers.contains(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())){
//                                uniqueOrderNumbers.add(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber());
//                                orders.add(deliveryManifestItem.getOrderItem().getOrdering());
//                            }
//                            ProductPrice productPrice = productPriceRepository.findByProductId(deliveryManifestItem.getProductId());
//                            Double totalPrice = null;
//                            if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "PORCENTAJE")){
//                                totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity())-((productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity())*(deliveryManifestItem.getOrderItem().getDiscountAmount()/100));
//                            }
//
//                            if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "MONTO")){
//                                totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity())-(deliveryManifestItem.getOrderItem().getDiscountAmount());
//                            }
//
//                            if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "NO APLICA")){
//                                totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity());
//                            }
//                            return DeliveryManifestItemDTO.builder()
//                                    .id(deliveryManifestItem.getId())
//                                    .user(deliveryManifestItem.getUser().getUsername())
//                                    .manifestNumber(deliveryManifestItem.getDeliveryManifest().getManifestNumber())
//                                    .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
//                                    .customer(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getName())
//                                    .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
//                                    .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
//                                    .quantity(deliveryManifestItem.getQuantity())
//                                    .skuProduct(iUtil.buildProductSku(deliveryManifestItem.getProduct()))
//                                    .management(deliveryManifestItem.getOrderItem().getOrdering().getManagementType().getName())
//                                    .paymentMethod(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentMethod().getName())
//                                    .paymentState(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentState().getName())
//                                    .orderItemAmount(totalPrice)
//                                    .build();
//                        }).toList();
//                double totalOrdersSaleAmount = 0.00;
//                double totalOrdersDuePayment = 0.00;
//                for(Ordering order:orders){
//                    List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(order.getId());
//                    double saleAmount = 0.00;
//                    for(OrderItem orderItem : orderItems){
//                        ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
//                        if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
//                            saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * orderItem.getQuantity()) * (orderItem.getDiscountAmount() / 100));
//                        }
//                        if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
//                            saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - orderItem.getDiscountAmount();
//                        }
//                        if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
//                            saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity());
//                        }
//                    }
//                    double totalDuePayment=0;
//                    if(Objects.equals(order.getDiscount().getName(), "PORCENTAJE")){
//                        totalDuePayment = (saleAmount-((saleAmount)*(order.getDiscountAmount()/100))+order.getDeliveryAmount())-order.getAdvancedPayment();
//                    }
//                    if(Objects.equals(order.getDiscount().getName(), "MONTO")){
//                        totalDuePayment = (saleAmount-order.getDiscountAmount()+order.getDeliveryAmount())-order.getAdvancedPayment();
//                    }
//                    if(Objects.equals(order.getDiscount().getName(), "NO APLICA")){
//                        totalDuePayment = (saleAmount+order.getDeliveryAmount())-order.getAdvancedPayment();
//                    }
//                    totalOrdersSaleAmount+=saleAmount;
//                    totalOrdersDuePayment+=totalDuePayment;
//                }
//                DeliveryManifestDTO deliveryManifestDTO =  DeliveryManifestDTO.builder()
//                        .id(deliveryManifest.getId())
//                        .user(deliveryManifest.getUser().getUsername())
//                        .manifestNumber(deliveryManifest.getManifestNumber())
//                        .id(deliveryManifest.getId())
//                        .open(deliveryManifest.getOpen())
//                        .courier(deliveryManifest.getCourier().getName())
//                        .warehouse(deliveryManifest.getWarehouse().getName())
//                        .registrationDate(deliveryManifest.getRegistrationDate())
//                        .updateDate(deliveryManifest.getUpdateDate())
//                        .deliveryManifestItemDTOS(deliveryManifestItemDTOS)
//                        .pickupAddress(deliveryManifest.getWarehouse().getAddress())
//                        .amount(totalOrdersSaleAmount)
//                        .paidAmount(totalOrdersSaleAmount-totalOrdersDuePayment)
//                        .payableAmount(totalOrdersDuePayment)
//                        .observations(deliveryManifest.getObservations())
//                        .build();
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                PdfWriter writer = new PdfWriter(out);
                PdfDocument pdfDoc = new PdfDocument(writer);

                Document document = new Document(pdfDoc);
                // Fonts
                PdfFont boldFont = PdfFontFactory.createFont(StandardFonts.HELVETICA_BOLD);
                PdfFont regularFont = PdfFontFactory.createFont(StandardFonts.HELVETICA);

                // Add Company Header
                document.add(new Paragraph("ARANNI®")
                        .setFont(boldFont)
                        .setFontSize(24)
                        .setTextAlignment(TextAlignment.CENTER));

                document.add(new Paragraph("IQUIQUE No.\nCel: 970334874\nCORPORACIÓN ARANNI SAC\nR.U.C: 20609605601")
                        .setFont(regularFont)
                        .setFontSize(10)
                        .setTextAlignment(TextAlignment.CENTER));

                document.add(new Paragraph("\n"));

                // Guide Number and Date Section
                Table guideTable = new Table(new float[]{2, 1});
                guideTable.setWidth(UnitValue.createPercentValue(100));

                guideTable.addCell(new Cell(1, 2)
                        .add(new Paragraph("GUIA DE SALIDA").setFont(boldFont).setTextAlignment(TextAlignment.CENTER))
                        .setBackgroundColor(new DeviceGray(0.85f))
                        .setPadding(5));

                guideTable.addCell(new Cell().add(new Paragraph("Número: #" + "001"))
                        .setFont(boldFont)
                        .setBorder(Border.NO_BORDER));

                guideTable.addCell(new Cell().add(new Paragraph("Fecha: " + "01/02/2025"))
                        .setFont(regularFont)
                        .setBorder(Border.NO_BORDER));

                document.add(guideTable);
                document.add(new Paragraph("\n"));

                // Driver Information
                document.add(new Paragraph("Placa: " + "0" + "    Cel: " + "3000000")
                        .setFont(regularFont)
                        .setFontSize(10));

                document.add(new Paragraph("\n"));

                // Paid Orders Table
                document.add(new Paragraph("PAGADOS").setFont(boldFont).setFontSize(12));

                Table paidTable = new Table(new float[]{1, 1, 3, 2, 2, 2});
                paidTable.setWidth(UnitValue.createPercentValue(100));

                paidTable.addCell("No. Pedido").setBold();
                paidTable.addCell("No. Paquetes").setBold();
                paidTable.addCell("Descripción").setBold();
                paidTable.addCell("Teléfono").setBold();
                paidTable.addCell("Distrito").setBold();
                paidTable.addCell("Importe").setBold();

                paidTable.addCell("32");
                paidTable.addCell("1");
                paidTable.addCell("Gabriela esther Anampa villanueva");
                paidTable.addCell("932840132");
                paidTable.addCell("San juan de lurigancho");
                paidTable.addCell("S/ " + "75.2");

                paidTable.addCell("192");
                paidTable.addCell("1");
                paidTable.addCell("Elisa puma robles");
                paidTable.addCell("932840132");
                paidTable.addCell("San juan de lurigancho");
                paidTable.addCell("S/ " + "67.2");

                paidTable.addCell("192");
                paidTable.addCell("1");
                paidTable.addCell("Soraida De la cruz ochante");
                paidTable.addCell("932840132");
                paidTable.addCell("San juan de lurigancho");
                paidTable.addCell("S/ " + "83.2");

                document.add(paidTable);
                document.add(new Paragraph("\n"));

                // COD Orders Table
                document.add(new Paragraph("CONTRAENTREGA").setFont(boldFont).setFontSize(12));

                Table cashOnDeliveryTable = new Table(new float[]{1, 1, 3, 2, 2, 2});
                cashOnDeliveryTable.setWidth(UnitValue.createPercentValue(100));

                cashOnDeliveryTable.addCell("No. Pedido").setBold();
                cashOnDeliveryTable.addCell("No. Paquetes").setBold();
                cashOnDeliveryTable.addCell("Descripción").setBold();
                cashOnDeliveryTable.addCell("Teléfono").setBold();
                cashOnDeliveryTable.addCell("Distrito").setBold();
                cashOnDeliveryTable.addCell("Importe").setBold();

                cashOnDeliveryTable.addCell("32");
                cashOnDeliveryTable.addCell("1");
                cashOnDeliveryTable.addCell("Gabriela esther Anampa villanueva");
                cashOnDeliveryTable.addCell("932840132");
                cashOnDeliveryTable.addCell("San juan de lurigancho");
                cashOnDeliveryTable.addCell("S/ " + "75.2");

                cashOnDeliveryTable.addCell("192");
                cashOnDeliveryTable.addCell("1");
                cashOnDeliveryTable.addCell("Elisa puma robles");
                cashOnDeliveryTable.addCell("932840132");
                cashOnDeliveryTable.addCell("San juan de lurigancho");
                cashOnDeliveryTable.addCell("S/ " + "67.2");

                cashOnDeliveryTable.addCell("192");
                cashOnDeliveryTable.addCell("1");
                cashOnDeliveryTable.addCell("Soraida De la cruz ochante");
                cashOnDeliveryTable.addCell("932840132");
                cashOnDeliveryTable.addCell("San juan de lurigancho");
                cashOnDeliveryTable.addCell("S/ " + "83.2");

                document.add(cashOnDeliveryTable);

                document.add(new Paragraph("\n"));

                // Summary Section
                Table summaryTable = new Table(new float[]{3, 1,3,1});
                summaryTable.setWidth(UnitValue.createPercentValue(100));

                summaryTable.addCell("No. PEDIDOS").setFont(boldFont);
                summaryTable.addCell("3").setTextAlignment(TextAlignment.RIGHT);

                summaryTable.addCell("TOTAL PAGADO").setFont(boldFont);
                summaryTable.addCell("S/ " + "0.00").setTextAlignment(TextAlignment.RIGHT);

                summaryTable.addCell("No. PEDIDOS").setFont(boldFont);
                summaryTable.addCell("0").setTextAlignment(TextAlignment.RIGHT);

                summaryTable.addCell("TOTAL POR COBRAR").setFont(boldFont);
                summaryTable.addCell("S/ " + "83.2").setTextAlignment(TextAlignment.RIGHT);

                document.add(summaryTable);

                document.add(new Paragraph("\n"));

                // Bank Details
                document.add(new Paragraph("BCP\nCta Cte: 191985539036\nCCI: 0021910098553903658\nYAPE ó PLIN: 970 334 874")
                        .setFont(regularFont)
                        .setFontSize(10));

                document.add(new Paragraph("\n"));

                // Signature Section
                Table signatureTable = new Table(3);
                signatureTable.setWidth(UnitValue.createPercentValue(100));

                signatureTable.addCell(new Cell().add(new Paragraph("DESPACHADO POR")).setBackgroundColor(new DeviceGray(0.85f)).setTextAlignment(TextAlignment.CENTER));
                signatureTable.addCell(new Cell().add(new Paragraph("RECIBIDO POR")).setBackgroundColor(new DeviceGray(0.85f)).setTextAlignment(TextAlignment.CENTER));
                signatureTable.addCell(new Cell().add(new Paragraph("VALIDADO POR")).setBackgroundColor(new DeviceGray(0.85f)).setTextAlignment(TextAlignment.CENTER));

                document.add(signatureTable);

//                for(DeliveryManifestItemDTO deliveryManifestItemDTO:deliveryManifestItemDTOS){
//                    table.addCell(deliveryManifestItemDTO.getOrderNumber().toString());
//                    table.addCell(deliveryManifestItemDTO.getCustomer());
//                    table.addCell(deliveryManifestItemDTO.getPhone());
//                    table.addCell(deliveryManifestItemDTO.getSkuProduct());
//                    table.addCell(deliveryManifestItemDTO.getDistrict());
//                    table.addCell(deliveryManifestItemDTO.getOrderItemAmount().toString());
//                    table.addCell(deliveryManifestItemDTO.getManagement());
//                }
//
//                // Populate Table with Items
//                for (DeliveryItem item : items) {
//                    table.addCell(item.getItemCode());
//                    table.addCell(item.getDescription());
//                    table.addCell(String.valueOf(item.getQuantity()));
//                    table.addCell(item.getDestination());
//                }
//                document.add(table);
                document.close();

                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
