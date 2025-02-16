package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IExcel;
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
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class TemplateImpl implements ITemplate {
    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final WarehouseRepository warehouseRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final OrderingRepository orderingRepository;
    private final OrderItemRepository orderItemRepository;
    private final OrderStockRepository orderStockRepository;
    private final OrderStockItemRepository orderStockItemRepository;
    private final OrderReturnTypeRepository orderReturnTypeRepository;
    private final BrandRepository brandRepository;
    private final ModelRepository modelRepository;
    private final ColorRepository colorRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final UnitTypeRepository unitTypeRepository;
    private final SizeRepository sizeRepository;
    private final UnitRepository unitRepository;
    private final SupplierRepository supplierRepository;
    private final ProductRepository productRepository;
    private final IExcel iExcel;
    @Override
    public CompletableFuture<ByteArrayInputStream> purchase(String supplierRuc, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Supplier supplier;
            List<SupplierProduct> supplierProductList;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(supplierRuc,user.getClientId());
            }

            if(supplier == null){
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }else{
                supplierProductList = supplierProductRepository.findAllByClientIdAndSupplierIdAndStatusTrue(user.getClientId(),supplier.getId());
            }

            if(supplierProductList.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            try{
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("compra");

                DataFormat format = workbook.createDataFormat();

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                headerStyle.setDataFormat(format.getFormat("@"));

                CellStyle headerStyle2 = workbook.createCellStyle();
                headerStyle2.setFillForegroundColor(IndexedColors.RED.getIndex());
                headerStyle2.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                headerStyle.setDataFormat(format.getFormat("@"));

                Row supplierRow = sheet.createRow(0);
                Cell supplierHeaderCell = supplierRow.createCell(0);
                supplierHeaderCell.setCellValue("PROVEEDOR");
                supplierHeaderCell.setCellStyle(headerStyle2);
                Cell supplierCell = supplierRow.createCell(1);
                supplierCell.setCellValue(supplier.getBusinessName());
                Cell supplierHeaderCell2 = supplierRow.createCell(2);
                supplierHeaderCell2.setCellValue("RUC");
                supplierHeaderCell2.setCellStyle(headerStyle2);
                Cell supplierCell2 = supplierRow.createCell(3);
                supplierCell2.setCellValue(supplier.getRuc());

                Row headerRow = sheet.createRow(1);

                Cell cell = headerRow.createCell(0);
                cell.setCellValue("MODELO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(1);
                cell.setCellValue("PRODUCTO SKU");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(2);
                cell.setCellValue("COLOR");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(3);
                cell.setCellValue("TIPO TALLA");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(4);
                cell.setCellValue("TALLA");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(5);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(6);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(7);
                cell.setCellValue("OBSERVACIONES");
                cell.setCellStyle(headerStyle);
                int currentRow = 2;
                for(SupplierProduct supplierProduct:supplierProductList){
                    Row row = sheet.createRow(currentRow);;
                    row.createCell(0).setCellValue(supplierProduct.getProduct().getModel().getName());
                    row.createCell(1).setCellValue(supplierProduct.getProduct().getSku());
                    row.createCell(2).setCellValue(supplierProduct.getProduct().getColor().getName());
                    row.createCell(3).setCellValue(supplierProduct.getProduct().getSize().getSizeType().getName());
                    row.createCell(4).setCellValue(supplierProduct.getProduct().getSize().getName());
                    row.createCell(5).setCellValue(supplierProduct.getSerial());
                    row.createCell(6).setCellValue(0);
                    row.createCell(7).setCellValue("NO APLICA");
                    currentRow++;
                }

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
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

                CellStyle headerStyle2 = workbook.createCellStyle();
                headerStyle2.setFillForegroundColor(IndexedColors.RED.getIndex());
                headerStyle2.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                XSSFSheet hiddenSheet1 = workbook.createSheet("Hidden1");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet1), true);

                String[] serialList = warehouseStockList.stream().map(warehouseStockItem -> warehouseStockItem.getSupplierProduct().getSerial()).toList().toArray(new String[0]);
                int rownum1 = 0;
                Row row1;
                Cell hiddenCell1;
                row1 = hiddenSheet1.createRow(rownum1++);
                int colnum1 = 0;
                for (String key : serialList) {
                    hiddenCell1 = row1.createCell(colnum1++);
                    hiddenCell1.setCellValue(key);
                }
                Name namedRange1 = workbook.createName();
                namedRange1.setNameName("WarehouseStock");
                String reference1 = "Hidden1!$A$1:" + iExcel.getExcelColumnReference('A',serialList.length-1) + "$1";
                namedRange1.setRefersToFormula(reference1);
                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelper.createFormulaListConstraint("WarehouseStock");
                CellRangeAddressList addressList = new CellRangeAddressList(1,quantity,0,0);
                DataValidation dataValidation = validationHelper.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);

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
    public CompletableFuture<ByteArrayInputStream> stockReturn(String warehouseName,String supplierRuc, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            Supplier supplier;
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
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(), warehouseName.toUpperCase());
                supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(supplierRuc,user.getClientId());
            }
            if(supplier==null){
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }
            if(warehouse==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }else{
                warehouseStockList = warehouseStockRepository.findByClientIdAndWarehouseIdAndSupplierProduct_Supplier_Id(user.getClientId(),warehouse.getId(),supplier.getId());
            }
            try{
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("devolucion_inventario");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                CellStyle headerStyle2 = workbook.createCellStyle();
                headerStyle2.setFillForegroundColor(IndexedColors.RED.getIndex());
                headerStyle2.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row supplierRow = sheet.createRow(0);
                Cell supplierHeaderCell = supplierRow.createCell(0);
                supplierHeaderCell.setCellValue("PROVEEDOR");
                supplierHeaderCell.setCellStyle(headerStyle2);
                Cell supplierCell = supplierRow.createCell(1);
                supplierCell.setCellValue(supplier.getBusinessName());
                Cell supplierHeaderCell2 = supplierRow.createCell(2);
                supplierHeaderCell2.setCellValue("RUC");
                supplierHeaderCell2.setCellStyle(headerStyle2);
                Cell supplierCell2 = supplierRow.createCell(3);
                supplierCell2.setCellValue(supplier.getRuc());
                Cell supplierHeaderCell3 = supplierRow.createCell(4);
                supplierHeaderCell3.setCellValue("ALMACEN");
                supplierHeaderCell3.setCellStyle(headerStyle2);
                Cell supplierCell3 = supplierRow.createCell(5);
                supplierCell3.setCellValue(warehouse.getName());

                Row headerRow = sheet.createRow(1);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(1);
                cell.setCellValue("SKU PRODUCTO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(2);
                cell.setCellValue("MODEL");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(3);
                cell.setCellValue("COLOR");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(4);
                cell.setCellValue("TALLA");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(5);
                cell.setCellValue("PROVEEDOR");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(6);
                cell.setCellValue("STOCK ACTUAL");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(7);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(8);
                cell.setCellValue("OBSERVACIONES");
                cell.setCellStyle(headerStyle);

                XSSFSheet hiddenSheet1 = workbook.createSheet("Hidden1");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet1), true);

                List<WarehouseStock> filteredWarehouseStockList = warehouseStockList.stream()
                        .filter(data -> data.getQuantity() > 0)
                        .toList();

                int currentRow = 2;
                for(WarehouseStock warehouseStock:filteredWarehouseStockList){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(warehouseStock.getSupplierProduct().getSerial());
                    row.createCell(1).setCellValue(warehouseStock.getSupplierProduct().getProduct().getSku());
                    row.createCell(2).setCellValue(warehouseStock.getSupplierProduct().getProduct().getModel().getName());
                    row.createCell(3).setCellValue(warehouseStock.getSupplierProduct().getProduct().getColor().getName());
                    row.createCell(4).setCellValue(warehouseStock.getSupplierProduct().getProduct().getSize().getName());
                    row.createCell(5).setCellValue(warehouseStock.getSupplierProduct().getSupplier().getBusinessName());
                    row.createCell(6).setCellValue(warehouseStock.getQuantity());
                    row.createCell(7).setCellValue(0);
                    row.createCell(8).setCellValue("NO APLICA");
                    currentRow++;
                }

                ByteArrayOutputStream out = new ByteArrayOutputStream();
                workbook.write(out);
                workbook.close();
                return new ByteArrayInputStream(out.toByteArray());
            }catch (RuntimeException | IOException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> stockReplenishment(UUID orderId, String username) throws BadRequestExceptions {
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

                CellStyle headerStyle2 = workbook.createCellStyle();
                headerStyle2.setFillForegroundColor(IndexedColors.RED.getIndex());
                headerStyle2.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                XSSFSheet hiddenSheet1 = workbook.createSheet("Hidden1");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet1), true);

                String[] serialList = supplierProductList.stream().map(SupplierProduct::getSerial).toList().toArray(new String[0]);
                int rownum1 = 0;
                Row row1;
                Cell hiddenCell1;
                row1 = hiddenSheet1.createRow(rownum1++);
                int colnum1 = 0;
                for (String key : serialList) {
                    hiddenCell1 = row1.createCell(colnum1++);
                    hiddenCell1.setCellValue(key);
                }
                Name namedRange1 = workbook.createName();
                namedRange1.setNameName("SupplierProducts");
                String reference1 = "Hidden1!$A$1:" + iExcel.getExcelColumnReference('A',serialList.length-1) + "$1";
                namedRange1.setRefersToFormula(reference1);
                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelper.createFormulaListConstraint("SupplierProducts");
                CellRangeAddressList addressList = new CellRangeAddressList(1,serialList.length,0,0);
                DataValidation dataValidation = validationHelper.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);

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
    public CompletableFuture<ByteArrayInputStream> orderStock(UUID orderId, String username) throws BadRequestExceptions {
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

                CellStyle headerStyle2 = workbook.createCellStyle();
                headerStyle2.setFillForegroundColor(IndexedColors.RED.getIndex());
                headerStyle2.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU PRODUCTO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(1);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle2);

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
                categoriesName.setRefersToFormula("Hidden!$A$1:$" + iExcel.getExcelColumnReference('A',orderStockMap.keySet().size() - 1) + "$1");

                for (int i = 0; i < orderStockMap.size(); i++) {
                    String category = (String) orderStockMap.keySet().toArray()[i];
                    Name name = workbook.createName();
                    name.setNameName("_"+category);
                    name.setRefersToFormula("Hidden!$B$" + (i + 2) + ":$"+iExcel.getExcelColumnReference('B',maxSubcatLength-1) + "$" + (i + 2));
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

            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ByteArrayInputStream> orderReturn(UUID orderId, String username) throws BadRequestExceptions {
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
                orderStock = orderStockRepository.findByOrderIdAndClientId(ordering.getId(),user.getClientId());
            }
            if(orderStock==null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }else{
                orderStockItemList = orderStockItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(user.getClientId(),ordering.getId());
            }

            try {
                int records = 0;
                for(OrderStockItem orderStockItem : orderStockItemList){
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

                CellStyle headerStyle2 = workbook.createCellStyle();
                headerStyle2.setFillForegroundColor(IndexedColors.RED.getIndex());
                headerStyle2.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU PRODUCTO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(1);
                cell.setCellValue("SKU INVENTARIO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(2);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(3);
                cell.setCellValue("TIPO");
                cell.setCellStyle(headerStyle2);

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
                categoriesName.setNameName("skus");
                categoriesName.setRefersToFormula("Hidden!$A$1:$" + iExcel.getExcelColumnReference('A',orderStockMap.keySet().size()) + "$1");

                for (int i = 0; i < orderStockMap.size(); i++) {
                    String category = (String) orderStockMap.keySet().toArray()[i];
                    Name name = workbook.createName();
                    name.setNameName("_"+category);
                    name.setRefersToFormula("Hidden!$B$" + (i + 2) + ":$" + iExcel.getExcelColumnReference('B',maxSubcatLength-1) +"$" + (i + 2));
                }

                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint categoryConstraint = validationHelper.createFormulaListConstraint("skus");
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

    @Override
    public CompletableFuture<ByteArrayInputStream> product(Integer quantity,String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<Brand> brands;
            List<Color> colors;
            List<CategoryProduct> categoryProducts;
            List<UnitType> unitTypes;
            Map<String,List<String>> modelMap = new HashMap<>();
            Map<String,List<String>> categoryProductSizeMap = new HashMap<>();
            Map<String,List<String>> unitMap = new HashMap<>();
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                colors = colorRepository.findAllByStatusTrue();
                categoryProducts = categoryProductRepository.findAllByStatusTrue();
                unitTypes = unitTypeRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                brands = brandRepository.findAllByClientIdAndStatusTrue(user.getClientId());
            }
            if(brands.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorBrand);
            }
            if(colors.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorColor);
            }
            if(categoryProducts.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorCategoryProduct);
            }
            if(unitTypes.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorUnitType);
            }
            try{

                for(Brand brand:brands){
                    List<Model> models = modelRepository.findAllByClientIdAndBrandIdAndStatusTrue(user.getClientId(), brand.getId());
                    if(models.isEmpty()){
                        throw new BadRequestExceptions(Constants.ErrorModel);
                    }
                    modelMap.put(brand.getName(),models.stream().map(Model::getName).toList());
                }

                for(CategoryProduct categoryProduct:categoryProducts){
                    List<Size> sizes = sizeRepository.findAllByStatusTrueAndSizeTypeId(categoryProduct.getSizeTypeId());
                    if(sizes.isEmpty()){
                        throw new BadRequestExceptions(Constants.ErrorSize);
                    }
                    categoryProductSizeMap.put(categoryProduct.getName(),sizes.stream().map(Size::getName).toList());
                }

                for(UnitType unitType:unitTypes){
                    List<Unit> units = unitRepository.findAllByUnitTypeIdAndStatusTrue(unitType.getId());
                    if(units.isEmpty()){
                        throw new BadRequestExceptions(Constants.ErrorUnit);
                    }
                    unitMap.put(unitType.getName(),units.stream().map(Unit::getName).toList());
                }

                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("productos");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                CellStyle headerStyle2 = workbook.createCellStyle();
                headerStyle2.setFillForegroundColor(IndexedColors.RED.getIndex());
                headerStyle2.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SKU PRODUCTO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("MARCA");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(2);
                cell.setCellValue("MODELO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(3);
                cell.setCellValue("COLOR");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(4);
                cell.setCellValue("CATEGORIA");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(5);
                cell.setCellValue("TAMAÑO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(6);
                cell.setCellValue("TIPO_UNIDAD");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(7);
                cell.setCellValue("UNIDAD");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(8);
                cell.setCellValue("CARACTERISTICAS");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(9);
                cell.setCellValue("PRECIO");
                cell.setCellStyle(headerStyle);

                // brands and models dependent validation lists
                XSSFSheet hiddenSheet1 = workbook.createSheet("Hidden1");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet1), true);

                int rownum1 = 0;
                Row row1;
                Cell hiddenCell1;

                row1 = hiddenSheet1.createRow(rownum1++);
                int colnum1 = 0;
                for (String key : modelMap.keySet()) {
                    hiddenCell1 = row1.createCell(colnum1++);
                    hiddenCell1.setCellValue(key);
                }

                int maxSubcatLength1 = 0;
                for (Map.Entry<String, List<String>> entry : modelMap.entrySet()) {
                    String key = entry.getKey();
                    List<String> subcatList = entry.getValue();

                    row1 = hiddenSheet1.createRow(rownum1++);
                    colnum1 = 0;
                    hiddenCell1 = row1.createCell(colnum1++);
                    hiddenCell1.setCellValue(key);

                    for (String subcat : subcatList) {
                        hiddenCell1 = row1.createCell(colnum1++);
                        hiddenCell1.setCellValue(subcat);
                    }

                    maxSubcatLength1 = Math.max(maxSubcatLength1, subcatList.size());
                }

                Name categoriesName1 = workbook.createName();
                categoriesName1.setNameName("brands");
                categoriesName1.setRefersToFormula("Hidden1!$A$1:$" + iExcel.getExcelColumnReference(
                        'A',
                        modelMap.keySet().size() - 1
                ) + "$1");

                for (int i = 0; i < modelMap.size(); i++) {
                    String category = (String) modelMap.keySet().toArray()[i];
                    Name name = workbook.createName();
                    name.setNameName(category);
                    name.setRefersToFormula("Hidden1!$B$" + (i + 2) + ":$" + iExcel.getExcelColumnReference('B',maxSubcatLength1-1) + "$" + (i + 2));
                }

                DataValidationHelper validationHelperBrand = sheet.getDataValidationHelper();
                DataValidationConstraint brandConstraint = validationHelperBrand.createFormulaListConstraint("brands");
                CellRangeAddressList brandAddressList = new CellRangeAddressList(1, quantity, 1, 1);
                DataValidation brandValidation = validationHelperBrand.createValidation(brandConstraint, brandAddressList);
                sheet.addValidationData(brandValidation);

                for (int i = 1; i <= quantity; i++) {
                    DataValidationConstraint modelConstraint = validationHelperBrand.createFormulaListConstraint("INDIRECT($B" + (i + 1) + ")");
                    CellRangeAddressList modelAddressList = new CellRangeAddressList(i, i, 2, 2);
                    DataValidation modelValidation = validationHelperBrand.createValidation(modelConstraint, modelAddressList);
                    sheet.addValidationData(modelValidation);
                }

                // size and size type dependent validation lists
                XSSFSheet hiddenSheet2 = workbook.createSheet("Hidden2");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet2), true);

                int rownum2 = 0;
                Row row2;
                Cell hiddenCell2;

                row2 = hiddenSheet2.createRow(rownum2++);
                int colnum2 = 0;
                for (String key : categoryProductSizeMap.keySet()) {
                    hiddenCell2 = row2.createCell(colnum2++);
                    hiddenCell2.setCellValue(key);
                }

                int maxSubcatLength2 = 0;
                for (Map.Entry<String, List<String>> entry : categoryProductSizeMap.entrySet()) {
                    String key = entry.getKey();
                    List<String> subcatList = entry.getValue();

                    row2 = hiddenSheet2.createRow(rownum2++);
                    colnum2 = 0;
                    hiddenCell2 = row2.createCell(colnum2++);
                    hiddenCell2.setCellValue(key);

                    for (String subcat : subcatList) {
                        hiddenCell2 = row2.createCell(colnum2++);
                        hiddenCell2.setCellValue(subcat);
                    }

                    maxSubcatLength2 = Math.max(maxSubcatLength2, subcatList.size());
                }

                Name categoriesName2 = workbook.createName();
                categoriesName2.setNameName("categories");
                categoriesName2.setRefersToFormula("Hidden2!$A$1:$" + iExcel.getExcelColumnReference('A',categoryProductSizeMap.keySet().size()-1) + "$1");

                for (int i = 0; i < categoryProductSizeMap.size(); i++) {
                    String category = (String) categoryProductSizeMap.keySet().toArray()[i];
                    Name name = workbook.createName();
                    name.setNameName(category);
                    name.setRefersToFormula("Hidden2!$B$" + (i + 2) + ":$" + iExcel.getExcelColumnReference('B',maxSubcatLength2-1) + "$" + (i + 2));
                }

                DataValidationHelper validationHelperCategory = sheet.getDataValidationHelper();
                DataValidationConstraint categoryConstraint = validationHelperCategory.createFormulaListConstraint("categories");
                CellRangeAddressList categoryAddressList = new CellRangeAddressList(1, quantity, 4, 4);
                DataValidation categoryValidation = validationHelperCategory.createValidation(categoryConstraint, categoryAddressList);
                sheet.addValidationData(categoryValidation);

                for (int i = 1; i <= quantity; i++) {
                    DataValidationConstraint sizeConstraint = validationHelperCategory.createFormulaListConstraint("INDIRECT($E" + (i + 1) + ")");
                    CellRangeAddressList sizeAddressList = new CellRangeAddressList(i, i, 5, 5);
                    DataValidation sizeValidation = validationHelperCategory.createValidation(sizeConstraint, sizeAddressList);
                    sheet.addValidationData(sizeValidation);
                }

                // unit and unit type dependent validation lists
                XSSFSheet hiddenSheet3 = workbook.createSheet("Hidden3");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet3), true);

                int rownum3 = 0;
                Row row3;
                Cell hiddenCell3;

                row3 = hiddenSheet3.createRow(rownum3++);
                int colnum3 = 0;
                for (String key : unitMap.keySet()) {
                    hiddenCell3 = row3.createCell(colnum3++);
                    hiddenCell3.setCellValue(key);
                }

                int maxSubcatLength3 = 0;
                for (Map.Entry<String, List<String>> entry : unitMap.entrySet()) {
                    String key = entry.getKey();
                    List<String> subcatList = entry.getValue();

                    row3 = hiddenSheet3.createRow(rownum3++);
                    colnum3 = 0;
                    hiddenCell3 = row3.createCell(colnum3++);
                    hiddenCell3.setCellValue(key);

                    for (String subcat : subcatList) {
                        hiddenCell3 = row3.createCell(colnum3++);
                        hiddenCell3.setCellValue(subcat);
                    }

                    maxSubcatLength3 = Math.max(maxSubcatLength3, subcatList.size());
                }

                Name categoriesName3 = workbook.createName();
                categoriesName3.setNameName("unit_types");
                categoriesName3.setRefersToFormula("Hidden3!$A$1:$" + iExcel.getExcelColumnReference('A',unitMap.keySet().size()-1) + "$1");

                for (int i = 0; i < unitMap.size(); i++) {
                    String category = (String) unitMap.keySet().toArray()[i];
                    System.out.println(category);
                    Name name = workbook.createName();
                    name.setNameName(category);
                    name.setRefersToFormula("Hidden3!$B$" + (i + 2) + ":$"+ iExcel.getExcelColumnReference('B',maxSubcatLength3-1) + "$" + (i + 2));
                }

                DataValidationHelper validationHelperUnitType = sheet.getDataValidationHelper();
                DataValidationConstraint unitTypeConstraint = validationHelperUnitType.createFormulaListConstraint("unit_types");
                CellRangeAddressList unitTypeAddressList = new CellRangeAddressList(1, quantity, 6, 6);
                DataValidation unitTypeValidation = validationHelperUnitType.createValidation(unitTypeConstraint, unitTypeAddressList);
                sheet.addValidationData(unitTypeValidation);

                for (int i = 1; i <= quantity; i++) {
                    DataValidationConstraint unitConstraint = validationHelperUnitType.createFormulaListConstraint("INDIRECT($G" + (i + 1) + ")");
                    CellRangeAddressList unitAddressList = new CellRangeAddressList(i, i, 7, 7);
                    DataValidation unitValidation = validationHelperUnitType.createValidation(unitConstraint, unitAddressList);
                    sheet.addValidationData(unitValidation);
                }

                XSSFSheet hiddenSheet4 = workbook.createSheet("Hidden4");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet4), true);

                // color validation list
                String[] colorArray = colors.stream().map(Color::getName).toList().toArray(new String[0]);
                int rownum4 = 0;
                Row row4;
                Cell hiddenCell4;
                row1 = hiddenSheet4.createRow(rownum4++);
                int colnum4 = 0;
                for (String key : colorArray) {
                    hiddenCell4 = row1.createCell(colnum4++);
                    hiddenCell4.setCellValue(key);
                }
                Name namedRange4 = workbook.createName();
                namedRange4.setNameName("Colors");
                String reference1 = "Hidden4!$A$1:" + iExcel.getExcelColumnReference('A',colorArray.length-1) + "$1";
                namedRange4.setRefersToFormula(reference1);
                DataValidationHelper validationHelperColor = sheet.getDataValidationHelper();
                DataValidationConstraint colorConstraint = validationHelperColor.createFormulaListConstraint("Colors");
                CellRangeAddressList colorAddressList = new CellRangeAddressList(1,quantity,3,3);
                DataValidation colorDataValidation = validationHelperColor.createValidation(colorConstraint,colorAddressList);
                sheet.addValidationData(colorDataValidation);

                CellStyle priceStyle = workbook.createCellStyle();
                DataFormat priceFormat = workbook.createDataFormat();
                priceStyle.setDataFormat(priceFormat.getFormat("_($* #,##0.00_);_($* (#,##0.00);_($* \"-\"??_);_(@_)"));

                for(int rowIndex = 1; rowIndex <= quantity;rowIndex++){
                    Row row = sheet.createRow(rowIndex);
                    Cell priceCell = row.createCell(9);
                    priceCell.setCellStyle(priceStyle);
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
    public CompletableFuture<ByteArrayInputStream> supplierProduct(Integer quantity, String username) throws BadRequestExceptions,InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<Product> products;
            List<Supplier> suppliers;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                products = productRepository.findAllByClientIdAndStatusTrue(user.getClientId());
                suppliers = supplierRepository.findAllByClientIdAndStatusTrue(user.getClientId());
            }
            if(products.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }
            if(suppliers.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }

            try{
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("productos_inventario");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                CellStyle headerStyle2 = workbook.createCellStyle();
                headerStyle2.setFillForegroundColor(IndexedColors.RED.getIndex());
                headerStyle2.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("SERIAL");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(1);
                cell.setCellValue("PRODUCTO");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(2);
                cell.setCellValue("PROVEEDOR");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(3);
                cell.setCellValue("PRECIO");
                cell.setCellStyle(headerStyle);


                XSSFSheet hiddenSheet1 = workbook.createSheet("Hidden1");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet1), true);

                // products
                String[] productList = products.stream().map(Product::getSku).toList().toArray(new String[0]);
                int rownum1 = 0;
                Row row1;
                Cell hiddenCell1;
                row1 = hiddenSheet1.createRow(rownum1++);
                int colnum1 = 0;
                for (String key : productList) {
                    hiddenCell1 = row1.createCell(colnum1++);
                    hiddenCell1.setCellValue(key);
                }
                Name namedRange1 = workbook.createName();
                namedRange1.setNameName("Products");
                String reference1 = "Hidden1!$A$1:" + iExcel.getExcelColumnReference('A',productList.length-1) + "$1";
                namedRange1.setRefersToFormula(reference1);

                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelper.createFormulaListConstraint("Products");
                CellRangeAddressList addressList = new CellRangeAddressList(1,quantity+1,1,1);
                DataValidation dataValidation = validationHelper.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);
                // suppliers
                String[] supplierList = suppliers.stream().map(Supplier::getBusinessName).toList().toArray(new String[0]);
                int rownum2 = 1;
                Row row2;
                Cell hiddenCell2;
                row2 = hiddenSheet1.createRow(rownum2++);
                int colnum2 = 0;
                for (String key : supplierList) {
                    hiddenCell2 = row2.createCell(colnum2++);
                    hiddenCell2.setCellValue(key);
                }
                Name namedRange2 = workbook.createName();
                namedRange2.setNameName("Suppliers");
                String reference2 = "Hidden1!$A$2:" + iExcel.getExcelColumnReference('A',productList.length-1) + "$2";
                namedRange2.setRefersToFormula(reference2);
                DataValidationHelper validationHelper2 = sheet.getDataValidationHelper();
                DataValidationConstraint constraint2 = validationHelper2.createFormulaListConstraint("Suppliers");
                CellRangeAddressList addressList2 = new CellRangeAddressList(1,quantity+1,2,2);
                DataValidation dataValidation2 = validationHelper2.createValidation(constraint2,addressList2);
                sheet.addValidationData(dataValidation2);

                CellStyle priceStyle = workbook.createCellStyle();
                DataFormat priceFormat = workbook.createDataFormat();
                priceStyle.setDataFormat(priceFormat.getFormat("_($* #,##0.00_);_($* (#,##0.00);_($* \"-\"??_);_(@_)"));

                for(int rowIndex = 1; rowIndex <= quantity;rowIndex++){
                    Row row = sheet.createRow(rowIndex);
                    Cell priceCell = row.createCell(3);
                    priceCell.setCellStyle(priceStyle);
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
    public CompletableFuture<ByteArrayInputStream> model(Integer quantity,String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            List<Brand> brands;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                brands = brandRepository.findAllByClientIdAndStatusTrue(user.getClientId());
            }
            if(brands.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorBrand);
            }
            try{
                XSSFWorkbook workbook = new XSSFWorkbook();
                XSSFSheet sheet = workbook.createSheet("modelos");

                CellStyle headerStyle = workbook.createCellStyle();
                headerStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                CellStyle headerStyle2 = workbook.createCellStyle();
                headerStyle2.setFillForegroundColor(IndexedColors.RED.getIndex());
                headerStyle2.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("MARCA");
                cell.setCellStyle(headerStyle2);

                cell = headerRow.createCell(1);
                cell.setCellValue("MODELO");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(2);
                cell.setCellValue("SKU");
                cell.setCellStyle(headerStyle);

                XSSFSheet hiddenSheet1 = workbook.createSheet("Hidden1");
                workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet1), true);

                // products
                String[] productList = brands.stream().map(Brand::getName).toList().toArray(new String[0]);
                int rownum1 = 0;
                Row row1;
                Cell hiddenCell1;
                row1 = hiddenSheet1.createRow(rownum1++);
                int colnum1 = 0;
                for (String key : productList) {
                    hiddenCell1 = row1.createCell(colnum1++);
                    hiddenCell1.setCellValue(key);
                }
                Name namedRange1 = workbook.createName();
                namedRange1.setNameName("Brands");
                String reference1 = "Hidden1!$A$1:" + iExcel.getExcelColumnReference('A',productList.length-1) + "$1";
                namedRange1.setRefersToFormula(reference1);
                DataValidationHelper validationHelper = sheet.getDataValidationHelper();
                DataValidationConstraint constraint = validationHelper.createFormulaListConstraint("Brands");
                CellRangeAddressList addressList = new CellRangeAddressList(1,quantity+1,0,0);
                DataValidation dataValidation = validationHelper.createValidation(constraint,addressList);
                sheet.addValidationData(dataValidation);

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
