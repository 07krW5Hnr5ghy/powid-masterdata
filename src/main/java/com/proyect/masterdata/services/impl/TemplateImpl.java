package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.ITemplate;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class TemplateImpl implements ITemplate {
    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final PurchaseRepository purchaseRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final ShipmentRepository shipmentRepository;
    private final ShipmentItemRepository shipmentItemRepository;
    private final OrderingRepository orderingRepository;
    private final OrderItemRepository orderItemRepository;
    private final OrderStockRepository orderStockRepository;
    private final OrderStockItemRepository orderStockItemRepository;
    private final OrderReturnTypeRepository orderReturnTypeRepository;
    @Override
    public CompletableFuture<ByteArrayInputStream> purchase(Integer quantity, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<SupplierProduct> supplierProductList;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                supplierProductList = supplierProductRepository.findAllByClientIdAndStatusTrue(user.getClientId());
            }

            if(supplierProductList.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            try{
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("compra");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                String[] serialList = supplierProductList.stream().map(SupplierProduct::getSerial).toList().toArray(new String[0]);
                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelper.createExplicitListConstraint(serialList);
                CellRangeAddressList addressList = new CellRangeAddressList(1,quantity,0,0);
                DataValidation dataValidation = validationHelper.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> shipment(Integer quantity, String purchaseSerial, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Purchase purchase;
            List<PurchaseItem> purchaseItemList;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                purchase = purchaseRepository.findBySerialAndStatusTrue(purchaseSerial.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(purchase==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }else{
                purchaseItemList = purchaseItemRepository.findAllByClientIdAndPurchaseIdAndStatusTrue(user.getClientId(),purchase.getId());
            }

            try{
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("embarque");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("OBSERVACIONES");
                cell.setCellStyle(headerStyle);

                String[] serialList = purchaseItemList.stream().map(purchaseItem -> purchaseItem.getSupplierProduct().getSerial()).toList().toArray(new String[0]);
                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelper.createExplicitListConstraint(serialList);
                CellRangeAddressList addressList = new CellRangeAddressList(1,quantity,0,0);
                DataValidation dataValidation = validationHelper.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> stockTransfer(Integer quantity, String warehouseName, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            List<WarehouseStock> warehouseStockList;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(),warehouseName.toUpperCase());
            }
            if(warehouse == null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }else{
                warehouseStockList = warehouseStockRepository.findAllByClientIdAndWarehouseId(user.getClientId(), warehouse.getId());
            }

            try {
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("transferencia");

                CellStyle style = workbook.createCellStyle();
                style.setFillBackgroundColor(IndexedColors.YELLOW.getIndex());
                style.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                String[] serialList = warehouseStockList.stream().map(warehouseStockItem -> warehouseStockItem.getSupplierProduct().getSerial()).toList().toArray(new String[0]);
                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelper.createExplicitListConstraint(serialList);
                CellRangeAddressList addressList = new CellRangeAddressList(1,quantity,0,0);
                DataValidation dataValidation = validationHelper.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> stockReturn(Integer quantity, String purchaseSerial, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Shipment shipment;
            List<ShipmentItem> shipmentItemList;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                shipment = shipmentRepository.findByPurchaseSerial(purchaseSerial.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(shipment==null){
                throw new BadRequestExceptions(Constants.ErrorShipment);
            }else{
                shipmentItemList = shipmentItemRepository.findAllByClientIdAndShipmentId(user.getClientId(), shipment.getId());
            }

            try{
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("devolucion_inventario");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("OBSERVACIONES");
                cell.setCellStyle(headerStyle);

                String[] serialList = shipmentItemList.stream().map(shipmentItem -> shipmentItem.getSupplierProduct().getSerial()).toList().toArray(new String[0]);
                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelper.createExplicitListConstraint(serialList);
                CellRangeAddressList addressList = new CellRangeAddressList(1,quantity,0,0);
                DataValidation dataValidation = validationHelper.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> stockReplenishment(Long orderId, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            List<OrderItem> orderItems;
            List<SupplierProduct> supplierProductList = new ArrayList<>();
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                ordering = orderingRepository.findByClientIdAndId(user.getClientId(), orderId);
            }
            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }else{
                orderItems = orderItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(user.getClientId(),ordering.getId());
            }
            try {
                orderItems.forEach(orderItem -> {
                    List<SupplierProduct> supplierProductInnerList = supplierProductRepository.findAllByClientIdAndProductIdAndStatusTrue(user.getClientId(), orderItem.getProductId());
                    supplierProductList.addAll(supplierProductInnerList);
                });
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("reposicion_inventario");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                String[] serialList = supplierProductList.stream().map(SupplierProduct::getSerial).toList().toArray(new String[0]);
                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelper.createExplicitListConstraint(serialList);
                CellRangeAddressList addressList = new CellRangeAddressList(1,serialList.length,0,0);
                DataValidation dataValidation = validationHelper.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> orderStock(Long orderId, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            List<OrderItem> orderItemList;
            Map<String,List<String>> orderStockMap = new HashMap<>();
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                ordering = orderingRepository.findByClientIdAndId(user.getClientId(), orderId);
            }
            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }else {
                orderItemList = orderItemRepository.findAllByOrderIdAndStatusTrue(ordering.getId());
            }
            try {
                int records = 0;
                for(OrderItem orderItem:orderItemList){
                    List<SupplierProduct> supplierProductList = supplierProductRepository.findAllByClientIdAndProductIdAndStatusTrue(user.getClientId(), orderItem.getProductId());
                    records += supplierProductList.size();
                    orderStockMap.put(orderItem.getProduct().getSku(),supplierProductList.stream().map(SupplierProduct::getSerial).toList());
                }
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("preparacion_pedido");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU PRODUCTO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                XSSFSheet hiddenSheet = workbook.createSheet("Hidden");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet), true);

                int rownum = 0;
                Row row;
                Cell hiddenCell;

                row = hiddenSheet.createRow(rownum++);
                int colnum = 0;
                for (String key : orderStockMap.keySet()) {
                    hiddenCell = row.createCell(colnum++);
                    hiddenCell.setCellValue("_"+key);
                }

                int maxSubcatLength = 0;
                for (Map.Entry<String, List<String>> entry : orderStockMap.entrySet()) {
                    String key = entry.getKey();
                    List<String> subcatList = entry.getValue();

                    row = hiddenSheet.createRow(rownum++);
                    colnum = 0;
                    hiddenCell = row.createCell(colnum++);
                    hiddenCell.setCellValue(key);

                    for (String subcat : subcatList) {
                        hiddenCell = row.createCell(colnum++);
                        hiddenCell.setCellValue(subcat);
                    }

                    maxSubcatLength = Math.max(maxSubcatLength, subcatList.size());
                }

                Name categoriesName = workbook.createName();
                categoriesName.setNameName("Categories");
                categoriesName.setRefersToFormula("Hidden!$A$1:$" + (char) ('A' + orderStockMap.keySet().size() - 1) + "$1");

                for (int i = 0; i < orderStockMap.size(); i++) {
                    String category = (String) orderStockMap.keySet().toArray()[i];
                    Name name = workbook.createName();
                    name.setNameName("_"+category);
                    name.setRefersToFormula("Hidden!$B$" + (i + 2) + ":$" + (char) ('B' + maxSubcatLength - 1) + "$" + (i + 2));
                }

                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint categoryConstraint = validationHelper.createFormulaListConstraint("Categories");
                CellRangeAddressList categoryAddressList = new CellRangeAddressList(1, records, 0, 0);
                DataValidation categoryValidation = validationHelper.createValidation(categoryConstraint, categoryAddressList);
                sheet.addValidationData(categoryValidation);

                for (int i = 1; i <= records; i++) {
                    DataValidationConstraint subcategoryConstraint = validationHelper.createFormulaListConstraint("INDIRECT($A" + (i + 1) + ")");
                    CellRangeAddressList subcategoryAddressList = new CellRangeAddressList(i, i, 1, 1);
                    DataValidation subcategoryValidation = validationHelper.createValidation(subcategoryConstraint, subcategoryAddressList);
                    sheet.addValidationData(subcategoryValidation);
                }

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();

                return new ByteArrayInputStream(out.toByteArray());

            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> orderReturn(Long orderId, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderStock orderStock;
            Map<String,List<String>> orderStockMap = new HashMap<>();
            List<OrderStockItem> orderStockItemList;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                ordering = orderingRepository.findByClientIdAndId(user.getClientId(),orderId);
            }
            if(ordering==null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }else{
                orderStock = orderStockRepository.findByOrderId(ordering.getId());
            }
            if(orderStock==null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }else{
                orderStockItemList = orderStockItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(user.getClientId(),ordering.getId());
            }

            try {
                int records = 0;
                for(OrderStockItem orderStockItem : orderStockItemList){
                    System.out.println(orderStockItem);
                    List<SupplierProduct> supplierProductList = supplierProductRepository.findAllByClientIdAndProductIdAndStatusTrue(user.getClientId(), orderStockItem.getOrderItem().getProductId());
                    records +=  supplierProductList.size();
                    orderStockMap.put(orderStockItem.getOrderItem().getProduct().getSku(),supplierProductList.stream().map(SupplierProduct::getSerial).toList());
                }
                List<OrderReturnType> orderReturnTypeList = orderReturnTypeRepository.findAllByStatusTrue();

                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("devolucion_pedido");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU PRODUCTO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("TIPO");
                cell.setCellStyle(headerStyle);

                XSSFSheet hiddenSheet = workbook.createSheet("Hidden");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet), true);

                int rownum = 0;
                Row row;
                Cell hiddenCell;

                row = hiddenSheet.createRow(rownum++);
                int colnum = 0;
                for (String key : orderStockMap.keySet()) {
                    hiddenCell = row.createCell(colnum++);
                    hiddenCell.setCellValue("_"+key);
                }

                int maxSubcatLength = 0;
                for (Map.Entry<String, List<String>> entry : orderStockMap.entrySet()) {
                    String key = entry.getKey();
                    List<String> subcatList = entry.getValue();

                    row = hiddenSheet.createRow(rownum++);
                    colnum = 0;
                    hiddenCell = row.createCell(colnum++);
                    hiddenCell.setCellValue(key);

                    for (String subcat : subcatList) {
                        hiddenCell = row.createCell(colnum++);
                        hiddenCell.setCellValue(subcat);
                    }

                    maxSubcatLength = Math.max(maxSubcatLength, subcatList.size());
                }

                Name categoriesName = workbook.createName();
                categoriesName.setNameName("Categories");
                categoriesName.setRefersToFormula("Hidden!$A$1:$" + (char) ('A' + orderStockMap.keySet().size() - 1) + "$1");

                for (int i = 0; i < orderStockMap.size(); i++) {
                    String category = (String) orderStockMap.keySet().toArray()[i];
                    Name name = workbook.createName();
                    name.setNameName("_"+category);
                    name.setRefersToFormula("Hidden!$B$" + (i + 2) + ":$" + (char) ('B' + maxSubcatLength - 1) + "$" + (i + 2));
                }

                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint categoryConstraint = validationHelper.createFormulaListConstraint("Categories");
                CellRangeAddressList categoryAddressList = new CellRangeAddressList(1, records, 0, 0);
                DataValidation categoryValidation = validationHelper.createValidation(categoryConstraint, categoryAddressList);
                sheet.addValidationData(categoryValidation);

                for (int i = 1; i <= records; i++) {
                    DataValidationConstraint subcategoryConstraint = validationHelper.createFormulaListConstraint("INDIRECT($A" + (i + 1) + ")");
                    CellRangeAddressList subcategoryAddressList = new CellRangeAddressList(i, i, 1, 1);
                    DataValidation subcategoryValidation = validationHelper.createValidation(subcategoryConstraint, subcategoryAddressList);
                    sheet.addValidationData(subcategoryValidation);
                }

                String[] orderReturnTypes = orderReturnTypeList.stream().map(OrderReturnType::getName).toList().toArray(new String[0]);
                DataValidationHelper validationHelperOrderType = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelperOrderType.createExplicitListConstraint(orderReturnTypes);
                CellRangeAddressList addressList = new CellRangeAddressList(1,records,3,3);
                DataValidation dataValidation = validationHelperOrderType.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();

                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
