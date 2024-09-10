package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.GeneralStock;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.WarehouseStock;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.GeneralStockRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseStockRepository;
import com.proyect.masterdata.services.IReport;
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
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class ReportImpl implements IReport {
    private final UserRepository userRepository;
    private final GeneralStockRepository generalStockRepository;
    private final WarehouseStockRepository warehouseStockRepository;
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
                cell.setCellValue("COLOR");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(4);
                cell.setCellValue("TALLA");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(5);
                cell.setCellValue("PROVEEDOR");
                cell.setCellStyle(headerStyle);

                cell = headerRow.createCell(6);
                cell.setCellValue("CANTIDAD");
                cell.setCellStyle(headerStyle);

                int currentRow = 1;
                for(GeneralStock generalStock:generalStockList){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(generalStock.getSupplierProduct().getSerial());
                    row.createCell(1).setCellValue(generalStock.getSupplierProduct().getProduct().getSku());
                    row.createCell(2).setCellValue(generalStock.getSupplierProduct().getProduct().getModel().getName());
                    row.createCell(3).setCellValue(generalStock.getSupplierProduct().getProduct().getColor().getName());
                    row.createCell(4).setCellValue(generalStock.getSupplierProduct().getProduct().getSize().getName());
                    row.createCell(5).setCellValue(generalStock.getSupplierProduct().getSupplier().getBusinessName());
                    row.createCell(6).setCellValue(generalStock.getQuantity());
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
                for(WarehouseStock warehouseStock:warehouseStockList){
                    Row row = sheet.createRow(currentRow);
                    row.createCell(0).setCellValue(warehouseStock.getSupplierProduct().getSerial());
                    row.createCell(1).setCellValue(warehouseStock.getSupplierProduct().getProduct().getSku());
                    row.createCell(2).setCellValue(warehouseStock.getWarehouse().getName());
                    row.createCell(3).setCellValue(warehouseStock.getSupplierProduct().getProduct().getModel().getName());
                    row.createCell(4).setCellValue(warehouseStock.getSupplierProduct().getProduct().getColor().getName());
                    row.createCell(5).setCellValue(warehouseStock.getSupplierProduct().getProduct().getSize().getName());
                    row.createCell(6).setCellValue(warehouseStock.getSupplierProduct().getSupplier().getBusinessName());
                    row.createCell(7).setCellValue(warehouseStock.getQuantity());
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
}
