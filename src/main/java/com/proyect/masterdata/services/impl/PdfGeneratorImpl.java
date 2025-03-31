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
import com.proyect.masterdata.dto.DeliveryManifestOrderDTO;
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
import java.util.stream.Collectors;

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
    private final DeliveryManifestOrderRepository deliveryManifestOrderRepository;
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
    public CompletableFuture<InputStream> generateDeliveryManifestReport(UUID deliveryManifestId, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryManifest deliveryManifest;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryManifest = deliveryManifestRepository.findById(deliveryManifestId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryManifest==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryStatusExists);
            }
            try{
                List<Ordering> orders = new ArrayList<>();
                Set<Long> uniqueOrderNumbers = new HashSet<>();
                List<DeliveryManifestOrderDTO> deliveryManifestOrderDTOS = new ArrayList<>();
                List<DeliveryManifestItemDTO> deliveryManifestItemDTOS = deliveryManifestItemRepository.findAllById(deliveryManifest.getId())
                        .stream().map(deliveryManifestItem -> {
                            if(!uniqueOrderNumbers.contains(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())){
                                uniqueOrderNumbers.add(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber());
                                orders.add(deliveryManifestItem.getOrderItem().getOrdering());
                            }
                            ProductPrice productPrice = productPriceRepository.findByProductId(deliveryManifestItem.getProductId());
                            Double totalPrice = null;
                            if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "PORCENTAJE")){
                                totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity())-((productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity())*(deliveryManifestItem.getOrderItem().getDiscountAmount()/100));
                            }

                            if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "MONTO")){
                                totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity())-(deliveryManifestItem.getOrderItem().getDiscountAmount());
                            }

                            if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "NO APLICA")){
                                totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity());
                            }
                            return DeliveryManifestItemDTO.builder()
                                    .id(deliveryManifestItem.getId())
                                    .delivered(deliveryManifestItem.getDelivered())
                                    .user(deliveryManifestItem.getUser().getUsername())
                                    .manifestNumber(deliveryManifestItem.getDeliveryManifest().getManifestNumber())
                                    .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
                                    .customer(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getName())
                                    .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
                                    .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
                                    .quantity(deliveryManifestItem.getQuantity())
                                    .skuProduct(iUtil.buildProductSku(deliveryManifestItem.getProduct()))
                                    .management(deliveryManifestItem.getOrderItem().getOrdering().getManagementType().getName())
                                    .paymentMethod(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentMethod().getName())
                                    .paymentState(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentState().getName())
                                    .orderItemAmount(totalPrice)
                                    .product(deliveryManifestItem.getProduct().getName())
                                    .build();
                        }).toList();
                double totalOrdersSaleAmount = 0.00;
                double totalOrdersDuePayment = 0.00;
                for(Ordering order:orders){
                    List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(order.getId());
                    double saleAmount = 0.00;
                    for(OrderItem orderItem : orderItems){
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
                    }
                    double totalDuePayment=0;
                    if(Objects.equals(order.getDiscount().getName(), "PORCENTAJE")){
                        totalDuePayment = (saleAmount-((saleAmount)*(order.getDiscountAmount()/100))+order.getDeliveryAmount())-order.getAdvancedPayment();
                    }
                    if(Objects.equals(order.getDiscount().getName(), "MONTO")){
                        totalDuePayment = (saleAmount-order.getDiscountAmount()+order.getDeliveryAmount())-order.getAdvancedPayment();
                    }
                    if(Objects.equals(order.getDiscount().getName(), "NO APLICA")){
                        totalDuePayment = (saleAmount+order.getDeliveryAmount())-order.getAdvancedPayment();
                    }
                    totalOrdersSaleAmount+=saleAmount;
                    totalOrdersDuePayment+=totalDuePayment;
                    DeliveryManifestOrderDTO deliveryManifestOrderDTO = DeliveryManifestOrderDTO.builder()
                            .address(order.getCustomer().getAddress())
                            .dni(order.getCustomer().getDni())
                            .customer(order.getCustomer().getName())
                            .orderNumber(order.getOrderNumber())
                            .orderState(order.getOrderState().getName())
                            .payableAmount(totalDuePayment)
                            .paymentMethod(order.getOrderPaymentMethod().getName())
                            .advancePayment(order.getAdvancedPayment())
                            .deliveryManifestItemDTOList(deliveryManifestItemDTOS.stream()
                                    .filter(item -> Objects.equals(item.getOrderNumber(), order.getOrderNumber())).toList())
                            .orderId(order.getId())
                            .build();
                    DeliveryManifestOrder deliveryManifestOrder = deliveryManifestOrderRepository.findByDeliveryManifestIdAndOrderIdAndClientId(
                            deliveryManifest.getId(),
                            order.getId(),
                            order.getClientId()
                    );
                    if(deliveryManifestOrder!=null){
                        deliveryManifestOrderDTO.setReceivedAmount(deliveryManifestOrder.getReceivedAmount());
                        deliveryManifestOrderDTO.setObservations(deliveryManifestOrder.getObservations());
                    }else{
                        deliveryManifestOrderDTO.setReceivedAmount(0.00);
                        deliveryManifestOrderDTO.setObservations("Sin observaciones");
                    }
                    deliveryManifestOrderDTOS.add(deliveryManifestOrderDTO);
                }
                DeliveryManifestDTO deliveryManifestDTO =  DeliveryManifestDTO.builder()
                        .id(deliveryManifest.getId())
                        .user(deliveryManifest.getUser().getUsername())
                        .manifestNumber(deliveryManifest.getManifestNumber())
                        .id(deliveryManifest.getId())
                        .open(deliveryManifest.getOpen())
                        .courier(deliveryManifest.getCourier().getName())
                        .warehouse(deliveryManifest.getWarehouse().getName())
                        .registrationDate(deliveryManifest.getRegistrationDate())
                        .updateDate(deliveryManifest.getUpdateDate())
                        .deliveryManifestOrderDTOS(deliveryManifestOrderDTOS)
                        .pickupAddress(deliveryManifest.getWarehouse().getAddress())
                        .amount(totalOrdersSaleAmount)
                        .paidAmount(totalOrdersSaleAmount-totalOrdersDuePayment)
                        .payableAmount(totalOrdersDuePayment)
                        .observations(deliveryManifest.getObservations())
                        .courierPhone(deliveryManifest.getCourier().getPhone())
                        .courierPlate(deliveryManifest.getCourier().getPlate())
                        .build();
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

                guideTable.addCell(new Cell().add(new Paragraph("Número: #" + deliveryManifestDTO.getManifestNumber()))
                        .setFont(boldFont)
                        .setBorder(Border.NO_BORDER));

                guideTable.addCell(new Cell().add(new Paragraph("Fecha: " + deliveryManifestDTO.getRegistrationDate()))
                        .setFont(regularFont)
                        .setBorder(Border.NO_BORDER));

                document.add(guideTable);
                document.add(new Paragraph("\n"));

                // Driver Information
                document.add(new Paragraph("Placa: " + deliveryManifestDTO.getCourierPlate() + "    Cel: " + deliveryManifestDTO.getCourierPhone())
                        .setFont(regularFont)
                        .setFontSize(10));

                document.add(new Paragraph("\n"));

                // Paid Orders Table
                document.add(new Paragraph("PAGADOS").setFont(boldFont).setFontSize(12));

                Table paidTable = new Table(new float[]{1, 2, 3, 2, 2, 2});
                paidTable.setWidth(UnitValue.createPercentValue(100));

                paidTable.addCell("No. Pedido").setBold();
                paidTable.addCell("Producto").setBold();
                paidTable.addCell("Descripción").setBold();
                paidTable.addCell("Teléfono").setBold();
                paidTable.addCell("Distrito").setBold();
                paidTable.addCell("Importe").setBold();

                List<DeliveryManifestItemDTO> paidItems = new ArrayList<>();
                double paidItemsAmount = 0;
                for(DeliveryManifestOrderDTO deliveryManifestOrderDTO:deliveryManifestDTO.getDeliveryManifestOrderDTOS()){
                    for(DeliveryManifestItemDTO deliveryManifestItemDTO:deliveryManifestOrderDTO.getDeliveryManifestItemDTOList()){
                        if(!Objects.equals(deliveryManifestItemDTO.getPaymentMethod(), "CONTRAENTREGA")){
                            paidTable.addCell(deliveryManifestItemDTO.getOrderNumber().toString());
                            paidTable.addCell(deliveryManifestItemDTO.getProduct());
                            paidTable.addCell(deliveryManifestItemDTO.getCustomer());
                            paidTable.addCell(deliveryManifestItemDTO.getPhone());
                            paidTable.addCell(deliveryManifestItemDTO.getDistrict());
                            paidTable.addCell("S/ " + deliveryManifestItemDTO.getOrderItemAmount());
                            paidItems.add(deliveryManifestItemDTO);
                            paidItemsAmount+=deliveryManifestItemDTO.getOrderItemAmount();
                        }
                    }
                }

                document.add(paidTable);
                document.add(new Paragraph("\n"));

                // COD Orders Table
                document.add(new Paragraph("CONTRAENTREGA").setFont(boldFont).setFontSize(12));

                Table cashOnDeliveryTable = new Table(new float[]{1, 2, 3, 2, 2, 2});
                cashOnDeliveryTable.setWidth(UnitValue.createPercentValue(100));

                cashOnDeliveryTable.addCell("No. Pedido").setBold();
                cashOnDeliveryTable.addCell("Producto").setBold();
                cashOnDeliveryTable.addCell("Descripción").setBold();
                cashOnDeliveryTable.addCell("Teléfono").setBold();
                cashOnDeliveryTable.addCell("Distrito").setBold();
                cashOnDeliveryTable.addCell("Importe").setBold();

                List<DeliveryManifestItemDTO> cashOnDeliveryItems = new ArrayList<>();
                double cashOnDeliveryItemsAmount = 0;
                for(DeliveryManifestOrderDTO deliveryManifestOrderDTO:deliveryManifestDTO.getDeliveryManifestOrderDTOS()){
                    for(DeliveryManifestItemDTO deliveryManifestItemDTO:deliveryManifestOrderDTO.getDeliveryManifestItemDTOList()){
                        if(Objects.equals(deliveryManifestItemDTO.getPaymentMethod(), "CONTRAENTREGA")){
                            cashOnDeliveryTable.addCell(deliveryManifestItemDTO.getOrderNumber().toString());
                            cashOnDeliveryTable.addCell(deliveryManifestItemDTO.getProduct());
                            cashOnDeliveryTable.addCell(deliveryManifestItemDTO.getCustomer());
                            cashOnDeliveryTable.addCell(deliveryManifestItemDTO.getPhone());
                            cashOnDeliveryTable.addCell(deliveryManifestItemDTO.getDistrict());
                            cashOnDeliveryTable.addCell("S/ " + deliveryManifestItemDTO.getOrderItemAmount());
                            cashOnDeliveryItems.add(deliveryManifestItemDTO);
                            cashOnDeliveryItemsAmount+=deliveryManifestItemDTO.getOrderItemAmount();
                        }
                    }
                }

                document.add(cashOnDeliveryTable);

                document.add(new Paragraph("\n"));

                Map<Long, Long> paidOrderCount = paidItems.stream()
                        .collect(Collectors.groupingBy(DeliveryManifestItemDTO::getOrderNumber, Collectors.counting()));

                Map<Long, Long> cashOnDeliveryOrderCount = cashOnDeliveryItems.stream()
                        .collect(Collectors.groupingBy(DeliveryManifestItemDTO::getOrderNumber, Collectors.counting()));

                // Summary Section
                Table summaryTable = new Table(new float[]{3, 1,3,1});
                summaryTable.setWidth(UnitValue.createPercentValue(100));

                summaryTable.addCell("No. PEDIDOS").setFont(boldFont);
                summaryTable.addCell(String.valueOf(paidOrderCount.size())).setTextAlignment(TextAlignment.RIGHT);

                summaryTable.addCell("TOTAL PAGADO").setFont(boldFont);
                summaryTable.addCell("S/ " + paidItemsAmount).setTextAlignment(TextAlignment.RIGHT);

                summaryTable.addCell("No. PEDIDOS").setFont(boldFont);
                summaryTable.addCell(String.valueOf(cashOnDeliveryOrderCount.size())).setTextAlignment(TextAlignment.RIGHT);

                summaryTable.addCell("TOTAL POR COBRAR").setFont(boldFont);
                summaryTable.addCell("S/ " + cashOnDeliveryItemsAmount).setTextAlignment(TextAlignment.RIGHT);

                document.add(summaryTable);

                document.add(new Paragraph("\n"));

                // Bank Details
                document.add(new Paragraph("BCP\nCta Cte: 191985539036\nCCI: 0021910098553903658\nYAPE ó PLIN: 970 334 874\nObservaciones:")
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

                document.close();

                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
