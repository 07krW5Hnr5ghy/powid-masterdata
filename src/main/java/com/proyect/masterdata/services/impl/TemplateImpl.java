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
import java.util.List;
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

                CellStyle style = workbook.createCellStyle();
                style.setFillBackgroundColor(IndexedColors.YELLOW.getIndex());
                style.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("INVENTARIO SKU");
                cell.setCellStyle(style);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(style);

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

                CellStyle style = workbook.createCellStyle();
                style.setFillBackgroundColor(IndexedColors.YELLOW.getIndex());
                style.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("INVENTARIO SKU");
                cell.setCellStyle(style);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(style);

                cell = headerRow.createCell(2);
                cell.setCellValue("OBSERVACIONES");
                cell.setCellStyle(style);

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

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("INVENTARIO SKU");
                cell.setCellStyle(style);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(style);

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

                CellStyle style = workbook.createCellStyle();
                style.setFillBackgroundColor(IndexedColors.YELLOW.getIndex());
                style.setFillPattern(FillPatternType.SOLID_FOREGROUND);

                Row headerRow = sheet.createRow(0);
                Cell cell = headerRow.createCell(0);
                cell.setCellValue("INVENTARIO SKU");
                cell.setCellStyle(style);

                cell = headerRow.createCell(1);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(style);

                cell = headerRow.createCell(2);
                cell.setCellValue("OBSERVACIONES");
                cell.setCellStyle(style);

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
}
