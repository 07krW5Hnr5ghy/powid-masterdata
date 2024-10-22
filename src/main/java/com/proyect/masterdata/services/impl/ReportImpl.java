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
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

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
    private final CustomerRepository customerRepository;
    private final ClosingChannelRepository closingChannelRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final DepartmentRepository departmentRepository;
    private final ProvinceRepository provinceRepository;
    private final DistrictRepository districtRepository;
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
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
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
                double totalDeliveredOrders = 0;
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
                            totalDeliveredOrders+=dailySaleSummaryDeliveredDTO.getTotalOrders();
                        }else{
                            row.createCell(3).setCellValue(0);
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
                finalRow.createCell(3).setCellValue(totalDeliveredOrders);
                finalRow.createCell(4).setCellValue(totalDeliveredSales);
                finalRow.getCell(4).setCellStyle(moneyStyle);
                finalRow.createCell(5).setCellValue(totalDeliveredSales/totalSales);
                finalRow.getCell(5).setCellStyle(percentageStyle);
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
    public CompletableFuture<ByteArrayInputStream> salesBySellerSummary(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<SalesBySellerDTO> salesBySellerDTOS;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try{
                List<SalesSellerReportRawDTO> salesSellerReportRawDTOS = new ArrayList<>();
                orderItemRepository.findOrderItemsWithSellerByDateRangeAndClientId(
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd,
                        user.getClientId()
                ).forEach(item->{
                    salesSellerReportRawDTOS.add(
                            SalesSellerReportRawDTO.builder()
                                    .orderId((Long) item[0])
                                    .registrationDate((Date) item[1])
                                    .orderDiscountAmount((Double) item[3])
                                    .orderDiscountName((String) item[4])
                                    .quantity((Integer) item[6])
                                    .orderItemDiscountAmount((Double) item[7])
                                    .orderItemDiscountName((String) item[8])
                                    .categoryName((String) item[9])
                                    .unitSalePrice((Double) item[10])
                                    .brandName((String) item[11])
                                    .closingChannelName((String) item[12])
                                    .seller((String) item[13])
                                    .department((String) item[14])
                                    .province((String) item[15])
                                    .district((String) item[16])
                                    .orderState((String) item[17])
                                    .orderItemStatus((Boolean) item[18])
                                    .build()
                    );
                });
                List<User> userList = userRepository.findAllByClientId(user.getClientId());
                List<CategoryProduct> categoryProductList = categoryProductRepository.findAll();
                List<Brand> brandList = brandRepository.findAllByClientId(user.getClientId());
                List<ClosingChannel> closingChannelList = closingChannelRepository.findAll();
                List<DistrictReportDTO> districtReportDTOS = new ArrayList<>();
                districtRepository.findDepartmentsProvincesDistricts().forEach(item->{
                    districtReportDTOS.add(DistrictReportDTO.builder()
                                    .department((String) item[0])
                                    .province((String) item[1])
                                    .district((String) item[2])
                            .build());
                });
                List<SalesBySellerFinalDTO> salesBySellerFinalDTOS = new ArrayList<>();

                for(User userData:userList){
                    for(DistrictReportDTO districtReportDTO:districtReportDTOS){
                        for(ClosingChannel closingChannel:closingChannelList){
                            for(CategoryProduct categoryProduct:categoryProductList){
                                for(Brand brand:brandList){
                                    SalesBySellerFinalDTO salesBySellerFinalDTO = SalesBySellerFinalDTO.builder().build();
                                    salesBySellerFinalDTO.setSeller(userData.getName() + " " + userData.getSurname());
                                    salesBySellerFinalDTO.setDepartment(districtReportDTO.getDepartment());
                                    salesBySellerFinalDTO.setProvince(districtReportDTO.getProvince());
                                    salesBySellerFinalDTO.setDistrict(districtReportDTO.getDistrict());
                                    salesBySellerFinalDTO.setClosingChannel(closingChannel.getName());
                                    salesBySellerFinalDTO.setCategory(categoryProduct.getName());
                                    salesBySellerFinalDTO.setBrand(brand.getName());
                                    salesBySellerFinalDTOS.add(salesBySellerFinalDTO);
                                }
                            }
                        }
                    }
                }
                for(SalesBySellerFinalDTO salesBySellerFinalDTO:salesBySellerFinalDTOS){
                    double totalSalesBySeller = 0.00;
                    int totalOrdersBySeller = 0;
                    int totalDeliveredOrdersBySeller = 0;
                    int totalProductsBySeller = 0;
                    for(SalesSellerReportRawDTO salesSellerReportRawDTO:salesSellerReportRawDTOS){
                        double orderSalesBySeller = 0.00;
                        int orderProductsBySeller = 0;
                        boolean orderFlag = false;
                        boolean deliveredOrderFlag = false;
                        if(Objects.equals(salesSellerReportRawDTO.getSeller(), salesBySellerFinalDTO.getSeller())){
                            if(Objects.equals(salesSellerReportRawDTO.getDepartment(), salesBySellerFinalDTO.getDepartment())){
                                if(Objects.equals(salesSellerReportRawDTO.getProvince(), salesBySellerFinalDTO.getProvince())){
                                    if(Objects.equals(salesSellerReportRawDTO.getDistrict(), salesBySellerFinalDTO.getDistrict())){
                                        if(Objects.equals(salesSellerReportRawDTO.getClosingChannelName(), salesBySellerFinalDTO.getClosingChannel())){
                                            if(Objects.equals(salesSellerReportRawDTO.getBrandName(), salesBySellerFinalDTO.getBrand())){
                                                if(Objects.equals(salesSellerReportRawDTO.getCategoryName(), salesBySellerFinalDTO.getCategory())){
                                                    if(salesSellerReportRawDTO.getOrderItemStatus()){
                                                        double totalPrice = 0.00;
                                                        if(Objects.equals(salesSellerReportRawDTO.getOrderItemDiscountName(), "PORCENTAJE")){
                                                            totalPrice = (salesSellerReportRawDTO.getUnitSalePrice() * salesSellerReportRawDTO.getQuantity())-((salesSellerReportRawDTO.getUnitSalePrice() * salesSellerReportRawDTO.getQuantity())*(salesSellerReportRawDTO.getOrderDiscountAmount()/100));
                                                        }

                                                        if(Objects.equals(salesSellerReportRawDTO.getOrderDiscountName(), "MONTO")){
                                                            totalPrice = (salesSellerReportRawDTO.getUnitSalePrice() * salesSellerReportRawDTO.getQuantity())-(salesSellerReportRawDTO.getOrderDiscountAmount());
                                                        }

                                                        if(Objects.equals(salesSellerReportRawDTO.getOrderDiscountName(), "NO APLICA")){
                                                            totalPrice = (salesSellerReportRawDTO.getUnitSalePrice() * salesSellerReportRawDTO.getQuantity());
                                                        }
                                                        if(Objects.equals(salesSellerReportRawDTO.getOrderState(), "ENTREGADO")){
                                                            deliveredOrderFlag = true;
                                                        }
                                                        orderFlag = true;
                                                        orderProductsBySeller+=salesSellerReportRawDTO.getQuantity();
                                                        orderSalesBySeller+=totalPrice;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        if(orderFlag){
                            totalOrdersBySeller++;
                        }
                        if(deliveredOrderFlag){
                            totalDeliveredOrdersBySeller++;
                        }
                        totalSalesBySeller += orderSalesBySeller;
                        totalProductsBySeller += orderProductsBySeller;
                    }
                    salesBySellerFinalDTO.setTotalSales(BigDecimal.valueOf(totalSalesBySeller).setScale(2,RoundingMode.HALF_EVEN));
                    salesBySellerFinalDTO.setTotalOrders(totalOrdersBySeller);
                    salesBySellerFinalDTO.setTotalProducts(totalProductsBySeller);
                    salesBySellerFinalDTO.setTotalDeliveredOrders(totalDeliveredOrdersBySeller);
                    if(salesBySellerFinalDTO.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00 && salesBySellerFinalDTO.getTotalProducts() > 0){
                        salesBySellerFinalDTO.setAverageTicket(BigDecimal.valueOf(totalProductsBySeller/totalOrdersBySeller).setScale(2,RoundingMode.HALF_EVEN));
                    }else{
                        salesBySellerFinalDTO.setAverageTicket(BigDecimal.valueOf(0.00));
                    }
                }
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
                cell.setCellValue("VENTAS TOTALES");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(8);
                cell.setCellValue("TOTAL PRODUCTOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(9);
                cell.setCellValue("TOTAL PEDIDOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(10);
                cell.setCellValue("TICKET PROMEDIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(11);
                cell.setCellValue("PEDIDOS ENTREGADOS");
                cell.setCellStyle(headerStyle);

                List<SalesBySellerFinalDTO> filteredValues = salesBySellerFinalDTOS.stream()
                        .filter(data -> data.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00)
                        .toList();

                int currentRow = 1;
                for(SalesBySellerFinalDTO salesBySellerDTO:filteredValues){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(salesBySellerDTO.getSeller());
                    row.createCell(1).setCellValue(salesBySellerDTO.getDepartment());
                    row.createCell(2).setCellValue(salesBySellerDTO.getProvince());
                    row.createCell(3).setCellValue(salesBySellerDTO.getDistrict());
                    row.createCell(4).setCellValue(salesBySellerDTO.getClosingChannel());
                    row.createCell(5).setCellValue(salesBySellerDTO.getCategory());
                    row.createCell(6).setCellValue(salesBySellerDTO.getBrand());
                    row.createCell(7).setCellValue(salesBySellerDTO.getTotalSales().doubleValue());
                    row.getCell(7).setCellStyle(moneyStyle);
                    row.createCell(8).setCellValue(salesBySellerDTO.getTotalProducts());
                    row.createCell(9).setCellValue(salesBySellerDTO.getTotalOrders());
                    row.createCell(10).setCellValue(salesBySellerDTO.getAverageTicket().doubleValue());
                    row.createCell(11).setCellValue(salesBySellerDTO.getTotalDeliveredOrders());
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
    public CompletableFuture<ByteArrayInputStream> salesByBrandSummary(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<Ordering> orderingList;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
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

                List<SalesByBrandDTO> filteredValues = salesByBrandDTOS.stream()
                        .filter(data -> data.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00)
                        .toList();

                int currentRow = 1;
                for(SalesByBrandDTO salesByBrandDTO:filteredValues){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(salesByBrandDTO.getSeller());
                    row.createCell(1).setCellValue(salesByBrandDTO.getBrand());
                    row.createCell(2).setCellValue(salesByBrandDTO.getTotalSales().doubleValue());
                    row.getCell(2).setCellStyle(moneyStyle);
                    row.createCell(3).setCellValue(salesByBrandDTO.getTotalOrders());
                    row.createCell(4).setCellValue(salesByBrandDTO.getTotalProducts());
                    row.createCell(5).setCellValue(salesByBrandDTO.getAverageTicket().doubleValue());
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
    public CompletableFuture<ByteArrayInputStream> dailySalesByBrandSummary(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<Ordering> orderingList;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try {
                List<SalesByBrandDailyDTO> salesByBrandDailyDTOS = new ArrayList<>();
                List<DailySaleSummaryDTO> orderDates = orderingRepository.findAllOrdersByDate(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd).stream().map(result -> DailySaleSummaryDTO.builder()
                        .date((Date) result[0])
                        .orderState("TODOS")
                        .totalOrders(((Long) result[1]).intValue())
                        .build()
                ).toList();
                orderingList = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd);
                List<User> userList = userRepository.findAllByClientId(user.getClientId());
                List<Brand> brandList = brandRepository.findAllByClientId(user.getClientId());
                for(User userData:userList){
                    for(Brand brand:brandList){
                        for(DailySaleSummaryDTO dailySaleSummaryDTO:orderDates){
                            SalesByBrandDailyDTO salesByBrandDailyDTO = SalesByBrandDailyDTO.builder().build();
                            salesByBrandDailyDTO.setBrand(brand.getName());
                            salesByBrandDailyDTO.setSeller(userData.getName() + " " + userData.getSurname());
                            salesByBrandDailyDTO.setDate(dailySaleSummaryDTO.getDate());
                            salesByBrandDailyDTOS.add(salesByBrandDailyDTO);
                        }
                    }
                }
                for(SalesByBrandDailyDTO salesByBrandDailyDTO:salesByBrandDailyDTOS){
                    double totalSalesByBrandAndSeller = 0.00;
                    int totalOrdersByBrandAndSeller = 0;
                    int totalProductsByBrandAndSeller = 0;
                    for(Ordering ordering:orderingList){
                        double orderSalesByBrandAndSeller = 0.00;
                        int orderProductsByBrandAndSeller = 0;
                        boolean orderFlag = false;
                        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                        String formattedDate = sdf.format(ordering.getRegistrationDate());
                        if(Objects.equals(salesByBrandDailyDTO.getDate().toString(), formattedDate)){
                            if(Objects.equals(ordering.getSeller(), salesByBrandDailyDTO.getSeller())){
                                List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                        user.getClientId(),
                                        ordering.getId());
                                for(OrderItem orderItem:orderItemList){
                                    if(Objects.equals(orderItem.getProduct().getModel().getBrand().getName(), salesByBrandDailyDTO.getBrand())){
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
                        }
                        if(orderFlag){
                            totalOrdersByBrandAndSeller++;
                        }
                        totalSalesByBrandAndSeller += orderSalesByBrandAndSeller;
                        totalProductsByBrandAndSeller += orderProductsByBrandAndSeller;
                    }
                    salesByBrandDailyDTO.setTotalSales(BigDecimal.valueOf(totalSalesByBrandAndSeller).setScale(2,RoundingMode.HALF_EVEN));
                    salesByBrandDailyDTO.setTotalOrders(totalOrdersByBrandAndSeller);
                    salesByBrandDailyDTO.setTotalProducts(totalProductsByBrandAndSeller);
                    if(salesByBrandDailyDTO.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00 && salesByBrandDailyDTO.getTotalProducts() > 0){
                        salesByBrandDailyDTO.setAverageTicket(BigDecimal.valueOf(totalProductsByBrandAndSeller/totalOrdersByBrandAndSeller).setScale(2,RoundingMode.HALF_EVEN));
                    }else{
                        salesByBrandDailyDTO.setAverageTicket(BigDecimal.valueOf(0.00));
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

                CellStyle dateStyle = workbook.createCellStyle();
                dateStyle.setDataFormat(format.getFormat("dd/mm/yyyy"));

                Row headerRow = sheet.createRow(0);

                Cell cell = headerRow.createCell(0);
                cell.setCellValue("FECHA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("VENDEDOR");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("MARCA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("VENTA TOTAL");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(4);
                cell.setCellValue("CANT PEDIDOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(5);
                cell.setCellValue("CANT PRODUCTOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(6);
                cell.setCellValue("TICKET PROMEDIO");
                cell.setCellStyle(headerStyle);

                List<SalesByBrandDailyDTO> filteredValues = salesByBrandDailyDTOS.stream()
                        .filter(data -> data.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00)
                        .toList();

                int currentRow = 1;
                for(SalesByBrandDailyDTO salesByBrandDailyDTO:filteredValues){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(salesByBrandDailyDTO.getDate());
                    row.getCell(0).setCellStyle(dateStyle);
                    row.createCell(1).setCellValue(salesByBrandDailyDTO.getSeller());
                    row.createCell(2).setCellValue(salesByBrandDailyDTO.getBrand());
                    row.createCell(3).setCellValue(salesByBrandDailyDTO.getTotalSales().doubleValue());
                    row.getCell(3).setCellStyle(moneyStyle);
                    row.createCell(4).setCellValue(salesByBrandDailyDTO.getTotalOrders());
                    row.createCell(5).setCellValue(salesByBrandDailyDTO.getTotalProducts());
                    row.createCell(6).setCellValue(salesByBrandDailyDTO.getAverageTicket().doubleValue());
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
    public CompletableFuture<ByteArrayInputStream> salesByStatusSummary(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<Ordering> orderingList;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try {
                List<SalesByStatusDTO> salesByStatusDTOS = new ArrayList<>();
                orderingList = orderingRepository.findByClientIdAndRegistrationDateBetween(
                        user.getClientId(),
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd);
                List<OrderState> orderStateList = orderStateRepository.findAll();
                List<Customer> customerList = customerRepository.findAllByClientId(user.getClientId());
                List<Brand> brandList = brandRepository.findAllByClientId(user.getClientId());
                for(OrderState orderState:orderStateList){
                    for(Customer customer:customerList){
                        for(Brand brand:brandList){
                            SalesByStatusDTO salesByStatusDTO = SalesByStatusDTO.builder().build();
                            salesByStatusDTO.setStatus(orderState.getName());
                            salesByStatusDTO.setCustomer(customer.getName());
                            salesByStatusDTO.setTelephone(customer.getPhone());
                            salesByStatusDTO.setBrand(brand.getName());
                            salesByStatusDTOS.add(salesByStatusDTO);
                        }
                    }
                }
                for(SalesByStatusDTO salesByStatusDTO:salesByStatusDTOS){
                    double totalSalesByBrandAndSeller = 0.00;
                    int totalOrdersByBrandAndSeller = 0;
                    int totalProductsByBrandAndSeller = 0;
                    for(Ordering ordering:orderingList){
                        double orderSalesByBrandAndSeller = 0.00;
                        int orderProductsByBrandAndSeller = 0;
                        boolean orderFlag = false;
                        if(Objects.equals(salesByStatusDTO.getStatus(), ordering.getOrderState().getName())){
                            if(Objects.equals(salesByStatusDTO.getTelephone(), ordering.getCustomer().getPhone())){
                                List<OrderItem> orderItemList = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(
                                        user.getClientId(),
                                        ordering.getId());
                                for(OrderItem orderItem:orderItemList){
                                    if(Objects.equals(orderItem.getProduct().getModel().getBrand().getName(), salesByStatusDTO.getBrand())){
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
                        }
                        if(orderFlag){
                            totalOrdersByBrandAndSeller++;
                        }
                        totalSalesByBrandAndSeller += orderSalesByBrandAndSeller;
                        totalProductsByBrandAndSeller += orderProductsByBrandAndSeller;
                    }
                    salesByStatusDTO.setTotalSales(BigDecimal.valueOf(totalSalesByBrandAndSeller).setScale(2,RoundingMode.HALF_EVEN));
                    salesByStatusDTO.setTotalOrders(totalOrdersByBrandAndSeller);
                    salesByStatusDTO.setTotalProducts(totalProductsByBrandAndSeller);
                    if(salesByStatusDTO.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00 && salesByStatusDTO.getTotalProducts() > 0){
                        salesByStatusDTO.setAverageTicket(BigDecimal.valueOf(totalProductsByBrandAndSeller/totalOrdersByBrandAndSeller).setScale(2,RoundingMode.HALF_EVEN));
                    }else{
                        salesByStatusDTO.setAverageTicket(BigDecimal.valueOf(0.00));
                    }
                }
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("ventas_status");

                DataFormat format = workbook.createDataFormat();

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                headerStyle.setDataFormat(format.getFormat("@"));

                CellStyle moneyStyle = workbook.createCellStyle();
                moneyStyle.setDataFormat(format.getFormat("$#,##0.00"));

                Row headerRow = sheet.createRow(0);

                Cell cell = headerRow.createCell(0);
                cell.setCellValue("FECHA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("CLIENTE");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("TELEFONO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("MARCA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(4);
                cell.setCellValue("VENTA TOTAL");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(5);
                cell.setCellValue("CANT PEDIDOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(6);
                cell.setCellValue("CANT PRODUCTOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(7);
                cell.setCellValue("TICKET PROMEDIO");
                cell.setCellStyle(headerStyle);

                List<SalesByStatusDTO> filteredValues = salesByStatusDTOS.stream()
                        .filter(data -> data.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00)
                        .toList();

                int currentRow = 1;
                for(SalesByStatusDTO salesByStatusDTO:filteredValues){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(salesByStatusDTO.getStatus());
                    row.createCell(1).setCellValue(salesByStatusDTO.getCustomer());
                    row.createCell(2).setCellValue(salesByStatusDTO.getTelephone());
                    row.createCell(3).setCellValue(salesByStatusDTO.getBrand());
                    row.createCell(4).setCellValue(salesByStatusDTO.getTotalSales().doubleValue());
                    row.getCell(4).setCellStyle(moneyStyle);
                    row.createCell(5).setCellValue(salesByStatusDTO.getTotalOrders());
                    row.createCell(6).setCellValue(salesByStatusDTO.getTotalProducts());
                    row.createCell(7).setCellValue(salesByStatusDTO.getAverageTicket().doubleValue());
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
    public CompletableFuture<ByteArrayInputStream> salesByCategory(Date registrationStartDate, Date registrationEndDate, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Date utcRegistrationDateStart;
            Date utcRegistrationDateEnd;
            List<Ordering> orderingList;
            List<SalesCategoryReportRawDTO> salesCategoryReportRawDTOS = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                utcRegistrationDateStart = iUtil.setToUTCStartOfDay(registrationStartDate);
                utcRegistrationDateEnd = iUtil.setToUTCEndOfDay(registrationEndDate);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try {
                List<SalesByCategoryDTO> salesByCategoryDTOS = new ArrayList<>();
                orderItemRepository.findOrderItemsWithBrandByDateRangeAndClientId(
                        utcRegistrationDateStart,
                        utcRegistrationDateEnd,
                        user.getClientId()
                ).forEach(item->{
                    salesCategoryReportRawDTOS.add(
                            SalesCategoryReportRawDTO.builder()
                                    .orderId((Long) item[0])
                                    .registrationDate((Date) item[1])
                                    .orderDiscountAmount((Double) item[3])
                                    .orderDiscountName((String) item[4])
                                    .quantity((Integer) item[6])
                                    .orderItemDiscountAmount((Double) item[7])
                                    .orderItemDiscountName((String) item[8])
                                    .categoryName((String) item[9])
                                    .unitSalePrice((Double) item[10])
                                    .brandName((String) item[11])
                                    .closingChannelName((String) item[12])
                                    .orderItemStatus((Boolean) item[13])
                                    .build()
                    );
                });
                List<ClosingChannel> closingChannelList = closingChannelRepository.findAll();
                List<CategoryProduct> categoryProductList = categoryProductRepository.findAll();
                List<Brand> brandList = brandRepository.findAllByClientId(user.getClientId());
                for(ClosingChannel closingChannel:closingChannelList){
                    for(CategoryProduct categoryProduct:categoryProductList){
                        for(Brand brand:brandList){
                            SalesByCategoryDTO salesByCategoryDTO = SalesByCategoryDTO.builder().build();
                            salesByCategoryDTO.setCategory(categoryProduct.getName());
                            salesByCategoryDTO.setBrand(brand.getName());
                            salesByCategoryDTO.setClosingChannel(closingChannel.getName());
                            salesByCategoryDTOS.add(salesByCategoryDTO);
                        }
                    }
                }
                for(SalesByCategoryDTO salesByCategoryDTO:salesByCategoryDTOS){
                    double totalSalesByCategoryAndBrandAndClosingChannel = 0.00;
                    int totalOrdersByCategoryAndBrandAndClosingChannel = 0;
                    int totalProductsByCategoryAndBrandAndClosingChannel = 0;
                    for(SalesCategoryReportRawDTO salesCategoryReportRawDTO:salesCategoryReportRawDTOS){
                        double orderSalesByCategoryAndBrandAndClosingChannel = 0.00;
                        int orderProductsByCategoryAndBrandAndClosingChannel = 0;
                        boolean orderFlag = false;
                        if(Objects.equals(salesByCategoryDTO.getClosingChannel(), salesCategoryReportRawDTO.getClosingChannelName())){
                            if(Objects.equals(salesCategoryReportRawDTO.getBrandName(), salesByCategoryDTO.getBrand())){
                                if(Objects.equals(salesCategoryReportRawDTO.getCategoryName(), salesByCategoryDTO.getCategory())){
                                    if(salesCategoryReportRawDTO.getOrderItemStatus()){
                                        double totalPrice = 0.00;
                                        if(Objects.equals(salesCategoryReportRawDTO.getOrderItemDiscountName(), "PORCENTAJE")){
                                            totalPrice = (salesCategoryReportRawDTO.getUnitSalePrice() * salesCategoryReportRawDTO.getQuantity())-((salesCategoryReportRawDTO.getUnitSalePrice() * salesCategoryReportRawDTO.getQuantity())*(salesCategoryReportRawDTO.getOrderItemDiscountAmount()/100));
                                        }
                                        if(Objects.equals(salesCategoryReportRawDTO.getOrderItemDiscountName(), "MONTO")){
                                            totalPrice = (salesCategoryReportRawDTO.getUnitSalePrice() * salesCategoryReportRawDTO.getQuantity())-(salesCategoryReportRawDTO.getOrderItemDiscountAmount());
                                        }
                                        if(Objects.equals(salesCategoryReportRawDTO.getOrderItemDiscountName(), "NO APLICA")){
                                            totalPrice = (salesCategoryReportRawDTO.getUnitSalePrice() * salesCategoryReportRawDTO.getQuantity());
                                        }
                                        orderFlag = true;
                                        orderProductsByCategoryAndBrandAndClosingChannel+=salesCategoryReportRawDTO.getQuantity();
                                        orderSalesByCategoryAndBrandAndClosingChannel+=totalPrice;
                                    }
                                }
                            }
                        }
                        if(orderFlag){
                            totalOrdersByCategoryAndBrandAndClosingChannel++;
                        }
                        totalSalesByCategoryAndBrandAndClosingChannel += orderSalesByCategoryAndBrandAndClosingChannel;
                        totalProductsByCategoryAndBrandAndClosingChannel += orderProductsByCategoryAndBrandAndClosingChannel;
                    }
                    salesByCategoryDTO.setTotalSales(BigDecimal.valueOf(totalSalesByCategoryAndBrandAndClosingChannel).setScale(2,RoundingMode.HALF_EVEN));
                    salesByCategoryDTO.setTotalOrders(totalOrdersByCategoryAndBrandAndClosingChannel);
                    salesByCategoryDTO.setTotalProducts(totalProductsByCategoryAndBrandAndClosingChannel);
                    if(salesByCategoryDTO.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00 && salesByCategoryDTO.getTotalProducts() > 0){
                        salesByCategoryDTO.setAverageTicket(BigDecimal.valueOf(totalProductsByCategoryAndBrandAndClosingChannel/totalOrdersByCategoryAndBrandAndClosingChannel).setScale(2,RoundingMode.HALF_EVEN));
                    }else{
                        salesByCategoryDTO.setAverageTicket(BigDecimal.valueOf(0.00));
                    }
                }
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("ventas_categoria");

                DataFormat format = workbook.createDataFormat();

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                headerStyle.setDataFormat(format.getFormat("@"));

                CellStyle moneyStyle = workbook.createCellStyle();
                moneyStyle.setDataFormat(format.getFormat("$#,##0.00"));

                Row headerRow = sheet.createRow(0);

                Cell cell = headerRow.createCell(0);
                cell.setCellValue("CANAL CIERRE");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("CATEGORIA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("MARCA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("VENTA TOTAL");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(4);
                cell.setCellValue("CANT PEDIDOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(5);
                cell.setCellValue("CANT PRODUCTOS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(6);
                cell.setCellValue("TICKET PROMEDIO");
                cell.setCellStyle(headerStyle);

                List<SalesByCategoryDTO> filteredValues = salesByCategoryDTOS.stream()
                        .filter(data -> data.getTotalSales().compareTo(BigDecimal.ZERO) > 0.00)
                        .toList();

                int currentRow = 1;
                for(SalesByCategoryDTO salesByCategoryDTO:filteredValues){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(salesByCategoryDTO.getClosingChannel());
                    row.createCell(1).setCellValue(salesByCategoryDTO.getCategory());
                    row.createCell(2).setCellValue(salesByCategoryDTO.getBrand());
                    row.createCell(3).setCellValue(salesByCategoryDTO.getTotalSales().doubleValue());
                    row.getCell(3).setCellStyle(moneyStyle);
                    row.createCell(4).setCellValue(salesByCategoryDTO.getTotalOrders());
                    row.createCell(5).setCellValue(salesByCategoryDTO.getTotalProducts());
                    row.createCell(6).setCellValue(salesByCategoryDTO.getAverageTicket().doubleValue());
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
