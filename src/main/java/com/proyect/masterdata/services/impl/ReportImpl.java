package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.DailySaleSummaryDTO;
import com.proyect.masterdata.dto.SalesBySellerDTO;
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

                cell = headerRow.createCell(4);
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
                            row.createCell(3).setCellValue(dailySaleSummaryDeliveredDTO.getTotalSalePerDay().doubleValue());
                            row.getCell(3).setCellStyle(moneyStyle);
                            totalDeliveredSales+=dailySaleSummaryDeliveredDTO.getTotalSalePerDay().doubleValue();
                            row.createCell(4).setCellValue(
                                    (
                                            dailySaleSummaryDeliveredDTO.getTotalSalePerDay().doubleValue()/
                                                    dailySaleSummaryDTO.getTotalSalePerDay().doubleValue())
                            );
                            row.getCell(4).setCellStyle(percentageStyle);
                        }else{
                            row.createCell(3).setCellValue(0.00);
                            row.getCell(3).setCellStyle(moneyStyle);
                            row.createCell(4).setCellValue(0.00);
                            row.getCell(4).setCellStyle(percentageStyle);
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
                salesBySellerDTOS = orderingRepository.findByClientIdAndRegistrationDateBetweenCountSellerDistrictChannel(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd
                ).stream().map(result -> SalesBySellerDTO.builder()
                        .seller(result[0].toString())
                        .department(result[1].toString())
                        .province(result[2].toString())
                        .district(result[3].toString())
                        .saleChannel(result[4].toString())
                        .totalOrders((long) result[5])
                        .build()
                ).toList();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
