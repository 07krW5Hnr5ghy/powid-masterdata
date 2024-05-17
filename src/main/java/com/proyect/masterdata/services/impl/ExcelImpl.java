package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestPurchaseExcel;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IExcel;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import static org.apache.poi.ss.usermodel.CellType.NUMERIC;
import static org.apache.poi.ss.usermodel.CellType.STRING;

@Service
@RequiredArgsConstructor
@Log4j2
public class ExcelImpl implements IExcel {
    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final PurchaseRepository purchaseRepository;
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    private final SupplierRepository supplierRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    @Override
    public CompletableFuture<ResponseSuccess> purchase(RequestPurchaseExcel requestPurchaseExcel,MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Purchase purchase;
            PurchaseDocument purchaseDocument;
            Supplier supplier;
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestPurchaseExcel.getTokenUser().toUpperCase());
                purchase = purchaseRepository.findBySerial(requestPurchaseExcel.getSerial().toUpperCase());
                purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchaseExcel.getDocumentName().toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                supplier = supplierRepository.findByClientIdAndRucAndStatusTrue(user.getClientId(), requestPurchaseExcel.getSupplierRuc().toUpperCase());
            }

            if(purchase != null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
            }

            if(purchaseDocument == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
            }

            try {
                Purchase newPurchase = purchaseRepository.save(Purchase.builder()
                        .serial(requestPurchaseExcel.getSerial().toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .purchaseDocument(purchaseDocument)
                        .purchaseDocumentId(purchaseDocument.getId())
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .build());
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                Map<Integer, List<String>> data = new HashMap<>();
                int i = 0;
                for(Row row : sheet){
                    PurchaseItem purchaseItem = PurchaseItem.builder().build();
                    SupplierProduct supplierProduct;
                    purchaseItem.setPurchase(newPurchase);
                    purchaseItem.setPurchaseId(newPurchase.getId());
                    data.put(i,new ArrayList<>());
                    System.out.println(i);
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType()==STRING)){
                            System.out.println("content serial");
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            purchaseItem.setSupplierProduct(supplierProduct);
                            purchaseItem.setSupplierProductId(supplierProduct.getId());
                            System.out.println(cell);
                        }

                        if(i>=1 && (cell.getCellType()==NUMERIC)){
                            System.out.println("quantity");
                            purchaseItem.setQuantity((int) cell.getNumericCellValue());
                            if(purchaseItem.getQuantity() == null){
                                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                            }
                            System.out.println(cell);
                        }

                        switch (cell.getCellType()){
                            case STRING :
                                data.get(i).add(cell.getRichStringCellValue().getString());
                                break;
                            case NUMERIC :
                                if(DateUtil.isCellDateFormatted(cell)){
                                    data.get(i).add(cell.getDateCellValue()+"");
                                }else{
                                    data.get(i).add(cell.getNumericCellValue()+"");
                                };
                                break;
                            case BOOLEAN:
                                data.get(i).add(cell.getBooleanCellValue()+"");
                                break;
                            case FORMULA:
                                data.get(i).add(cell.getCellFormula()+"");
                                break;
                            default:
                                data.get(i).add(" ");
                        }
                    }
                    if(i>=1){
                        purchaseItem.setStatus(true);
                        purchaseItem.setClient(user.getClient());
                        purchaseItem.setClientId(user.getClientId());
                        purchaseItem.setRegistrationDate(new Date(System.currentTimeMillis()));
                        purchaseItem.setTokenUser(user.getUsername());
                        purchaseItemRepository.save(purchaseItem);
                    }
                    i++;
                }
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
