package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.*;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IReport;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class ReportImpl implements IReport {
    private final UserRepository userRepository;
    private final GeneralStockRepository generalStockRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final OrderingRepository orderingRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductPriceRepository productPriceRepository;
    private final OrderStateRepository orderStateRepository;
    private final BrandRepository brandRepository;
    private final IUtil iUtil;
    @Override
    public CompletableFuture<ByteArrayInputStream> generalStockReport(String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<GeneralStock> generalStockList;
            try{
                user=userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                generalStockList = generalStockRepository.findAllByClientId(user.getClientId());
            }
            try {
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("inventario_general");

                DataFormat format = workbook.createDataFormat();

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                headerStyle.setDataFormat(format.getFormat("@"));

                Row headerRow = sheet.createRow(0);

                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("PRODUCTO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("MODELO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("CATEGORIA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(4);
                cell.setCellValue("COLOR");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(5);
                cell.setCellValue("TALLA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(6);
                cell.setCellValue("PROVEEDOR");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(7);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                int currentRow = 1;
                for(GeneralStock generalStock:generalStockList){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(generalStock.getSupplierProduct().getSerial());
                    row.createCell(1).setCellValue(generalStock.getSupplierProduct().getProduct().getSku());
                    row.createCell(2).setCellValue(generalStock.getSupplierProduct().getProduct().getModel().getName());
                    row.createCell(3).setCellValue(generalStock.getSupplierProduct().getProduct().getCategoryProduct().getName());
                    row.createCell(4).setCellValue(generalStock.getSupplierProduct().getProduct().getColor().getName());
                    row.createCell(5).setCellValue(generalStock.getSupplierProduct().getProduct().getSize().getName());
                    row.createCell(6).setCellValue(generalStock.getSupplierProduct().getSupplier().getBusinessName());
                    row.createCell(7).setCellValue(generalStock.getQuantity());
                    currentRow++;
                }
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> warehouseStockReport(String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<WarehouseStock> warehouseStockList;
            try{
                user=userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                warehouseStockList = warehouseStockRepository.findAllByClientId(user.getClientId());
            }
            try {
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("inventario_almacen");

                DataFormat format = workbook.createDataFormat();

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                headerStyle.setDataFormat(format.getFormat("@"));

                Row headerRow = sheet.createRow(0);

                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("PRODUCTO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("ALMACEN");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("MODELO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(4);
                cell.setCellValue("CATEGORIA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(5);
                cell.setCellValue("COLOR");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(6);
                cell.setCellValue("TALLA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(7);
                cell.setCellValue("PROVEEDOR");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(8);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                int currentRow = 1;
                for(WarehouseStock warehouseStock:warehouseStockList){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(warehouseStock.getSupplierProduct().getSerial());
                    row.createCell(1).setCellValue(warehouseStock.getSupplierProduct().getProduct().getSku());
                    row.createCell(2).setCellValue(warehouseStock.getWarehouse().getName());
                    row.createCell(3).setCellValue(warehouseStock.getSupplierProduct().getProduct().getModel().getName());
                    row.createCell(4).setCellValue(warehouseStock.getSupplierProduct().getProduct().getCategoryProduct().getName());
                    row.createCell(5).setCellValue(warehouseStock.getSupplierProduct().getProduct().getColor().getName());
                    row.createCell(6).setCellValue(warehouseStock.getSupplierProduct().getProduct().getSize().getName());
                    row.createCell(7).setCellValue(warehouseStock.getSupplierProduct().getSupplier().getBusinessName());
                    row.createCell(8).setCellValue(warehouseStock.getQuantity());
                    currentRow++;
                }
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> dailySalesSummary(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()-> {
            User user;
            List<Ordering> orderingListByDate;
            List<Ordering> orderingListByDateAndStatus;
            OrderState orderState;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusTrue("ENTREGADO");
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCStartOfDay(registrationEndDate);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            } else {
                orderingListByDate = orderingRepository.findByClientIdAndRegistrationDateBetween(user.getClientId(), registrationStartDate, registrationEndDate);
            }
            if(orderState==null){
                throw new BadRequestExceptions(Constants.ErrorOrderState);
            }else{
                orderingListByDateAndStatus = orderingRepository.findByClientIdAndRegistrationDateBetweenAndOrderStateId(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd,
                        orderState.getId()
                );
            }
            try{
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("resumen");

                DataFormat format = workbook.createDataFormat();

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                headerStyle.setDataFormat(format.getFormat("@"));

                CellStyle dateStyle = workbook.createCellStyle();
                dateStyle.setDataFormat(format.getFormat("dd/mm/yyyy"));

                CellStyle percentageStyle = workbook.createCellStyle();
                percentageStyle.setDataFormat(format.getFormat("0.00%"));

                CellStyle moneyStyle = workbook.createCellStyle();
                moneyStyle.setDataFormat(format.getFormat("$#,##0.00"));

                Row headerRow = sheet.createRow(0);

                Cell cell = headerRow.createCell(0);
                cell.setCellValue("FECHA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("VENTAS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("PEDIDOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("PEDIDOS ENTREGADOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(4);
                cell.setCellValue("VENTAS ENTREGADOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(5);
                cell.setCellValue("PORCENTAJE ENTREGADAS");
                cell.setCellStyle(headerStyle);

                List<DailySaleSummaryDTO> dailySaleSummaryDTOS = new ArrayList<>();
                List<DailySaleSummaryDTO> orderDates = orderingRepository.findAllOrdersByDate(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd).stream().map(result -> DailySaleSummaryDTO.builder()
                        .date((Date) result[0])
                        .orderState("TODOS")
                        .totalOrders(((Long) result[1]).intValue())
                        .build()
                ).toList();

                List<DailySaleSummaryDTO> dailySaleSummaryDeliveredDTOS = new ArrayList<>();
                List<DailySaleSummaryDTO> orderDatesDelivered = orderingRepository.findOrderCountByDateAndStatus(
                        user.getClientId(),
                        orderState.getId(),
                        registrationStartDate,
                        registrationEndDate).stream().map(result -> DailySaleSummaryDTO.builder()
                        .orderState(orderState.getName())
                        .date((Date) result[0])
                        .totalOrders(((Long) result[1]).intValue())
                        .build()
                ).toList();

                for(DailySaleSummaryDTO dailySaleSummaryDTO:orderDates){
                    double dailyTotalSales = 0.00;
                    for(Ordering ordering:orderingListByDate){
                        double orderTotalSales = 0.00;
                        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                        String formattedDate = sdf.format(ordering.getRegistrationDate());
                        if(dailySaleSummaryDTO.getDate().toString().equals(formattedDate)){
                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                    user.getClientId(),
                                    ordering.getId()
                            );
                            for(OrderItem orderItem:orderItemList){
                                ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                double totalPrice = 0.00;
                                if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                }
                                orderTotalSales+=totalPrice;
                            }
                        }
                        dailyTotalSales+=orderTotalSales;
                    }
                    dailySaleSummaryDTO.setTotalSalePerDay(BigDecimal.valueOf(dailyTotalSales).setScale(2, RoundingMode.HALF_EVEN));
                    dailySaleSummaryDTOS.add(dailySaleSummaryDTO);
                }
                for(DailySaleSummaryDTO dailySaleSummaryDeliveredDTO:orderDatesDelivered){
                    double dailyTotalSales = 0.00;
                    for(Ordering ordering:orderingListByDateAndStatus){
                        double orderTotalSales = 0.00;
                        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                        String formattedDate = sdf.format(ordering.getRegistrationDate());
                        if(dailySaleSummaryDeliveredDTO.getDate().toString().equals(formattedDate)){
                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                    user.getClientId(),
                                    ordering.getId()
                            );
                            for(OrderItem orderItem:orderItemList){
                                ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                double totalPrice = 0.00;
                                if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                }

                                if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                    totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                }
                                orderTotalSales+=totalPrice;
                            }
                            dailyTotalSales+=orderTotalSales;
                        }
                    }
                    dailySaleSummaryDeliveredDTO.setTotalSalePerDay(BigDecimal.valueOf(dailyTotalSales).setScale(2,RoundingMode.HALF_EVEN));
                    dailySaleSummaryDeliveredDTOS.add(dailySaleSummaryDeliveredDTO);
                }
                int currentRow = 1;
                double totalSales = 0.00;
                double totalDeliveredSales = 0.00;
                int totalOrders = 0;
                for(DailySaleSummaryDTO dailySaleSummaryDTO:dailySaleSummaryDTOS){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(dailySaleSummaryDTO.getDate());
                    row.getCell(0).setCellStyle(dateStyle);
                    row.createCell(1).setCellValue(dailySaleSummaryDTO.getTotalSalePerDay().doubleValue());
                    row.getCell(1).setCellStyle(moneyStyle);
                    totalSales += dailySaleSummaryDTO.getTotalSalePerDay().doubleValue();
                    row.createCell(2).setCellValue(dailySaleSummaryDTO.getTotalOrders());
                    totalOrders += dailySaleSummaryDTO.getTotalOrders();
                    for(DailySaleSummaryDTO dailySaleSummaryDeliveredDTO:dailySaleSummaryDeliveredDTOS){
                        if(Objects.equals(dailySaleSummaryDeliveredDTO.getDate().toString(), dailySaleSummaryDTO.getDate().toString())){
                            row.createCell(3).setCellValue(dailySaleSummaryDeliveredDTO.getTotalOrders());
                            row.createCell(4).setCellValue(dailySaleSummaryDeliveredDTO.getTotalSalePerDay().doubleValue());
                            row.getCell(4).setCellStyle(moneyStyle);
                            totalDeliveredSales+=dailySaleSummaryDeliveredDTO.getTotalSalePerDay().doubleValue();
                            row.createCell(5).setCellValue(
                                    (
                                            dailySaleSummaryDeliveredDTO.getTotalSalePerDay().doubleValue()/
                                                    dailySaleSummaryDTO.getTotalSalePerDay().doubleValue())
                            );
                            row.getCell(5).setCellStyle(percentageStyle);
                        }else{
                            row.createCell(3).setCellValue(dailySaleSummaryDeliveredDTO.getTotalOrders());
                            row.createCell(4).setCellValue(0.00);
                            row.getCell(4).setCellStyle(moneyStyle);
                            row.createCell(5).setCellValue(0.00);
                            row.getCell(5).setCellStyle(percentageStyle);
                        }
                    }
                    currentRow++;
                }
                Row finalRow = sheet.createRow(dailySaleSummaryDTOS.size()+1);
                finalRow.createCell(1).setCellValue(totalSales);
                finalRow.getCell(1).setCellStyle(moneyStyle);
                finalRow.createCell(2).setCellValue(totalOrders);
                finalRow.createCell(3).setCellValue(totalDeliveredSales);
                finalRow.getCell(3).setCellStyle(moneyStyle);
                finalRow.createCell(4).setCellValue(totalDeliveredSales/totalSales);
                finalRow.getCell(4).setCellStyle(percentageStyle);
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> SalesBySellerSummary(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<SalesBySellerDTO> salesBySellerDTOS;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCStartOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try{
                List<Ordering> orderingList = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd);
                salesBySellerDTOS = orderingRepository.findByClientIdAndRegistrationDateBetweenCountSellerReport(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd
                ).stream().map(result -> SalesBySellerDTO.builder()
                        .seller(result[0].toString())
                        .department(result[1].toString())
                        .province(result[2].toString())
                        .district(result[3].toString())
                        .closingChannel(result[4].toString())
                        .totalOrders((long) result[5])
                        .build()
                ).toList();
                System.out.println(salesBySellerDTOS);
                List<SalesBySellerFinalDTO> salesBySellerFinalDTOS = new ArrayList<>();

                for(SalesBySellerDTO salesBySellerDTO:salesBySellerDTOS){
                    int totalOrdersPerRecord = 0;
                    int totalDeliveredOrderPerRecord = 0;
                    SalesBySellerFinalDTO salesBySellerFinalDTOCurrent = SalesBySellerFinalDTO.builder().build();
                    for(Ordering ordering:orderingList){
                        if(Objects.equals(salesBySellerDTO.getSeller(), ordering.getSeller())){
                            salesBySellerFinalDTOCurrent.setSeller(salesBySellerDTO.getSeller());
                            if(Objects.equals(salesBySellerDTO.getDepartment(), ordering.getCustomer().getDistrict().getProvince().getDepartment().getName())){
                                salesBySellerFinalDTOCurrent.setDepartment(salesBySellerDTO.getDepartment());
                                if(Objects.equals(salesBySellerDTO.getProvince(), ordering.getCustomer().getDistrict().getProvince().getName())){
                                    salesBySellerFinalDTOCurrent.setProvince(salesBySellerDTO.getProvince());
                                    if(Objects.equals(salesBySellerDTO.getDistrict(), ordering.getCustomer().getDistrict().getName())){
                                        salesBySellerFinalDTOCurrent.setDistrict(salesBySellerDTO.getDistrict());
                                        if(Objects.equals(salesBySellerDTO.getClosingChannel(), ordering.getClosingChannel().getName())){
                                            salesBySellerFinalDTOCurrent.setClosingChannel(ordering.getClosingChannel().getName());
                                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                                    user.getClientId(),
                                                    ordering.getId());
                                            for(OrderItem orderItem:orderItemList){
                                                if(salesBySellerFinalDTOCurrent.getCategory()==null){
                                                    salesBySellerFinalDTOCurrent.setCategory(orderItem.getProduct().getCategoryProduct().getName());
                                                }
                                                if(salesBySellerFinalDTOCurrent.getBrand()==null){
                                                    salesBySellerFinalDTOCurrent.setBrand(orderItem.getProduct().getModel().getBrand().getName());
                                                }
                                                if(salesBySellerFinalDTOCurrent.getModel()==null){
                                                    salesBySellerFinalDTOCurrent.setModel(orderItem.getProduct().getModel().getName());
                                                }
                                                if(Objects.equals(salesBySellerFinalDTOCurrent.getCategory(), orderItem.getProduct().getCategoryProduct().getName())){

                                                    if(Objects.equals(salesBySellerFinalDTOCurrent.getBrand(), orderItem.getProduct().getModel().getBrand().getName())){
                                                        double totalSalesPerRecord = 0;
                                                        int totalProducts = 0;
                                                        if(Objects.equals(salesBySellerFinalDTOCurrent.getModel(), orderItem.getProduct().getModel().getName())){
                                                            ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                                            double totalPrice = 0.00;
                                                            if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                                                totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                                            }

                                                            if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                                                totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                                            }

                                                            if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                                                totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                                            }

                                                            if(Objects.equals(ordering.getOrderState().getName(), "ENTREGADO")){
                                                                totalDeliveredOrderPerRecord += 1;
                                                            }
                                                            totalOrdersPerRecord += 1;
                                                            totalSalesPerRecord += totalPrice;
                                                            totalProducts += orderItem.getQuantity();
                                                        }
                                                        salesBySellerFinalDTOCurrent.setTotalOrders(totalOrdersPerRecord);
                                                        salesBySellerFinalDTOCurrent.setTotalSales(BigDecimal.valueOf(totalSalesPerRecord).setScale(2,RoundingMode.HALF_EVEN));
                                                        salesBySellerFinalDTOCurrent.setTotalProducts(totalProducts);
                                                        if(salesBySellerFinalDTOCurrent.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00 && salesBySellerFinalDTOCurrent.getTotalProducts() > 0){
                                                            salesBySellerFinalDTOCurrent.setAverageTicket(BigDecimal.valueOf(totalProducts/totalOrdersPerRecord).setScale(2,RoundingMode.HALF_EVEN));
                                                        }else{
                                                            salesBySellerFinalDTOCurrent.setAverageTicket(BigDecimal.valueOf(0.00));
                                                        }
                                                        salesBySellerFinalDTOCurrent.setAverageTicket(BigDecimal.valueOf(totalProducts/totalOrdersPerRecord));
                                                        salesBySellerFinalDTOCurrent.setTotalDeliveredOrders(totalDeliveredOrderPerRecord);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    salesBySellerFinalDTOS.add(salesBySellerFinalDTOCurrent);
                }
                System.out.println(salesBySellerFinalDTOS);
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("ventas_vendedor");

                DataFormat format = workbook.createDataFormat();

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                headerStyle.setDataFormat(format.getFormat("@"));

                CellStyle moneyStyle = workbook.createCellStyle();
                moneyStyle.setDataFormat(format.getFormat("$#,##0.00"));

                Row headerRow = sheet.createRow(0);

                Cell cell = headerRow.createCell(0);
                cell.setCellValue("VENDEDOR");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("DEPARTAMENTO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("PROVINCIA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("DISTRITO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(4);
                cell.setCellValue("CANAL CIERRE");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(5);
                cell.setCellValue("CATEGORIA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(6);
                cell.setCellValue("MARCA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(7);
                cell.setCellValue("MODEL");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(8);
                cell.setCellValue("VENTAS TOTALES");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(9);
                cell.setCellValue("TOTAL PRODUCTOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(10);
                cell.setCellValue("TOTAL PEDIDOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(11);
                cell.setCellValue("TICKET PROMEDIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(12);
                cell.setCellValue("PEDIDOS ENTREGADOS");
                cell.setCellStyle(headerStyle);

                int currentRow = 1;
                for(SalesBySellerFinalDTO salesBySellerDTO:salesBySellerFinalDTOS){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(salesBySellerDTO.getSeller());
                    row.createCell(1).setCellValue(salesBySellerDTO.getDepartment());
                    row.createCell(2).setCellValue(salesBySellerDTO.getProvince());
                    row.createCell(3).setCellValue(salesBySellerDTO.getDistrict());
                    row.createCell(4).setCellValue(salesBySellerDTO.getClosingChannel());
                    row.createCell(5).setCellValue(salesBySellerDTO.getCategory());
                    row.createCell(6).setCellValue(salesBySellerDTO.getBrand());
                    row.createCell(7).setCellValue(salesBySellerDTO.getModel());
                    row.createCell(8).setCellValue(salesBySellerDTO.getTotalSales().doubleValue());
                    row.getCell(8).setCellStyle(moneyStyle);
                    row.createCell(9).setCellValue(salesBySellerDTO.getTotalProducts());
                    row.createCell(10).setCellValue(salesBySellerDTO.getTotalOrders());
                    row.createCell(11).setCellValue(salesBySellerDTO.getAverageTicket().doubleValue());
                    row.createCell(12).setCellValue(salesBySellerDTO.getTotalDeliveredOrders());
                    currentRow++;
                }

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> SalesByBrandSummary(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<Ordering> orderingList;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCStartOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try {
                List<SalesByBrandDTO> salesByBrandDTOS = new ArrayList<>();
                orderingList = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd);
                List<User> userList = userRepository.findAllByClientId(user.getClientId());
                List<Brand> brandList = brandRepository.findAllByClientId(user.getClientId());
                for(User userData:userList){
                    for(Brand brand:brandList){
                        SalesByBrandDTO salesByBrandDTO = SalesByBrandDTO.builder().build();
                        salesByBrandDTO.setBrand(brand.getName());
                        salesByBrandDTO.setSeller(userData.getName() + " " + userData.getSurname());
                        salesByBrandDTOS.add(salesByBrandDTO);
                    }
                }
                for(SalesByBrandDTO salesByBrandDTO:salesByBrandDTOS){
                    double totalSalesByBrandAndSeller = 0.00;
                    int totalOrdersByBrandAndSeller = 0;
                    int totalProductsByBrandAndSeller = 0;
                    for(Ordering ordering:orderingList){
                        double orderSalesByBrandAndSeller = 0.00;
                        int orderProductsByBrandAndSeller = 0;
                        boolean orderFlag = false;
                        if(Objects.equals(ordering.getSeller(), salesByBrandDTO.getSeller())){
                            List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                    user.getClientId(),
                                    ordering.getId());
                            for(OrderItem orderItem:orderItemList){
                                if(Objects.equals(orderItem.getProduct().getModel().getBrand().getName(), salesByBrandDTO.getBrand())){
                                    ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                                    double totalPrice = 0.00;
                                    if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                        totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                                    }

                                    if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                        totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                                    }

                                    if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                        totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                                    }
                                    orderFlag = true;
                                    orderProductsByBrandAndSeller+=orderItem.getQuantity();
                                    orderSalesByBrandAndSeller+=totalPrice;
                                }
                            }
                        }
                        if(orderFlag){
                            totalOrdersByBrandAndSeller++;
                        }
                        totalSalesByBrandAndSeller += orderSalesByBrandAndSeller;
                        totalProductsByBrandAndSeller += orderProductsByBrandAndSeller;
                    }
                    salesByBrandDTO.setTotalSales(BigDecimal.valueOf(totalSalesByBrandAndSeller).setScale(2,RoundingMode.HALF_EVEN));
                    salesByBrandDTO.setTotalOrders(totalOrdersByBrandAndSeller);
                    salesByBrandDTO.setTotalProducts(totalProductsByBrandAndSeller);
                    if(salesByBrandDTO.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00 && salesByBrandDTO.getTotalProducts() > 0){
                        salesByBrandDTO.setAverageTicket(BigDecimal.valueOf(totalProductsByBrandAndSeller/totalOrdersByBrandAndSeller).setScale(2,RoundingMode.HALF_EVEN));
                    }else{
                        salesByBrandDTO.setAverageTicket(BigDecimal.valueOf(0.00));
                    }
                }
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("ventas_marca");

                DataFormat format = workbook.createDataFormat();

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                headerStyle.setDataFormat(format.getFormat("@"));

                CellStyle moneyStyle = workbook.createCellStyle();
                moneyStyle.setDataFormat(format.getFormat("$#,##0.00"));

                Row headerRow = sheet.createRow(0);

                Cell cell = headerRow.createCell(0);
                cell.setCellValue("VENDEDOR");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("MARCA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("VENTA TOTAL");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("CANT PEDIDOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(4);
                cell.setCellValue("CANT PRODUCTOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(5);
                cell.setCellValue("TICKET PROMEDIO");
                cell.setCellStyle(headerStyle);

                int currentRow = 1;
                for(SalesByBrandDTO salesByBrandDTO:salesByBrandDTOS){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(salesByBrandDTO.getSeller());
                    row.createCell(1).setCellValue(salesByBrandDTO.getBrand());
                    row.createCell(2).setCellValue(salesByBrandDTO.getTotalSales().doubleValue());
                    row.getCell(2).setCellStyle(moneyStyle);
                    row.createCell(3).setCellValue(salesByBrandDTO.getTotalOrders());
                    row.createCell(4).setCellValue(salesByBrandDTO.getTotalProducts());
                    row.createCell(5).setCellValue(salesByBrandDTO.getAverageTicket().doubleValue());
                    row.getCell(5).setCellStyle(moneyStyle);
                    currentRow++;
                }

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
