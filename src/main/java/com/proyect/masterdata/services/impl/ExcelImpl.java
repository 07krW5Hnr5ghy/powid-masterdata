package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.dto.request.*;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.poi.ss.usermodel.*;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import static org.apache.poi.ss.usermodel.CellType.NUMERIC;
import static org.apache.poi.ss.usermodel.CellType.STRING;

@Service
@RequiredArgsConstructor
@Log4j2
public class ExcelImpl implements IExcel {
    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    private final SupplierRepository supplierRepository;
    private final PurchaseRepository purchaseRepository;
    private final PurchaseTypeRepository purchaseTypeRepository;
    private final WarehouseRepository warehouseRepository;
    private final StockReturnRepository stockReturnRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final IStockTransaction iStockTransaction;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final StockTransferRepository stockTransferRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final StockTransferItemRepository stockTransferItemRepository;
    private final StockReturnItemRepository stockReturnItemRepository;
    private final StockReplenishmentRepository stockReplenishmentRepository;
    private final OrderingRepository orderingRepository;
    private final ProductRepository productRepository;
    private final OrderItemRepository orderItemRepository;
    private final StockReplenishmentItemRepository stockReplenishmentItemRepository;
    private final OrderReturnRepository orderReturnRepository;
    private final OrderStockRepository orderStockRepository;
    private final OrderStockItemRepository orderStockItemRepository;
    private final OrderReturnTypeRepository orderReturnTypeRepository;
    private final OrderReturnItemRepository orderReturnItemRepository;
    private final IOrderStockItem iOrderStockItem;
    private final ModelRepository modelRepository;
    private final ColorRepository colorRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final SizeRepository sizeRepository;
    private final UnitRepository unitRepository;
    private final IAudit iAudit;
    private final ProductPriceRepository productPriceRepository;
    private final UnitTypeRepository unitTypeRepository;
    private final BrandRepository brandRepository;
    @Override
    public CompletableFuture<ResponseSuccess> purchase(RequestPurchaseExcel requestPurchaseExcel, MultipartFile multipartFile) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            Purchase purchase;
            PurchaseType purchaseType = null;
            StockReturn stockReturn;
            Supplier supplier;
            PurchaseDocument purchaseDocument;
            try {
                user = userRepository.findByUsernameAndStatusTrue(requestPurchaseExcel.getTokenUser().toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestPurchaseExcel.getWarehouse().toUpperCase());
                purchaseType = purchaseTypeRepository.findByNameAndStatusTrue(requestPurchaseExcel.getPurchaseType().toUpperCase());
                purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchaseExcel.getPurchaseDocument().toUpperCase());
            } catch (RuntimeException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (warehouse == null) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if(purchaseType == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseType);
            }else{
                purchase = purchaseRepository.findBySerialAndPurchaseTypeId(requestPurchaseExcel.getSerial(), purchaseType.getId());
            }

            if(purchase != null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(purchaseDocument == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
            }

            try {

                if(Objects.equals(purchaseType.getName(), "DEVOLUCION")){
                    stockReturn = stockReturnRepository.findBySerial(requestPurchaseExcel.getSerial());
                    if(stockReturn == null){
                        throw new BadRequestExceptions(Constants.ErrorStockReturn);
                    }
                }

                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                Row supplierRow = sheet.getRow(0);
                Cell supplierCell = supplierRow.getCell(3);
                supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(supplierCell.getRichStringCellValue().getString().toUpperCase(),user.getClientId());
                if(supplier == null){
                    throw new BadRequestExceptions(Constants.ErrorSupplier);
                }
                int i = 0;
                List<RequestStockTransactionItem> stockTransactionItemList = new ArrayList<>();
                List<RequestPurchaseItem> requestPurchaseItemList = new ArrayList<>();
                for(Row row:sheet){
                    SupplierProduct supplierProduct;
                    PurchaseItem purchaseItem;
                    RequestPurchaseItem requestPurchaseItem = RequestPurchaseItem.builder().build();
                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=2 && (cell.getCellType() == STRING) && (ii == 4)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestPurchaseItem.setSupplierProduct(supplierProduct.getSerial());
                        }
                        if(i>=2 && (cell.getCellType() == NUMERIC) && (ii == 4)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestPurchaseItem.setSupplierProduct(String.valueOf((int)(cell.getNumericCellValue())));
                        }
                        if(i>=2 && (cell.getCellType() == NUMERIC) && (ii==5)){
                            if(((int) cell.getNumericCellValue()) > 0){
                                requestPurchaseItem.setQuantity((int) cell.getNumericCellValue());
                            }
                        }
                        if(i>=2 && (cell.getCellType()==STRING) && (ii==6)){
                            requestPurchaseItem.setObservations(cell.getRichStringCellValue().getString().toUpperCase());
                        }
                        if(requestPurchaseItem.getObservations()==null){
                            requestPurchaseItem.setObservations("NO APLICA");
                        }
                        ii++;
                    }
                    if(i>=2 && (
                            requestPurchaseItem.getQuantity() != null &&
                            requestPurchaseItem.getQuantity() > 0 &&
                                    requestPurchaseItem.getSupplierProduct() != null)){
                        requestPurchaseItemList.add(requestPurchaseItem);
                    }
                    if(i>=2 && (
                            requestPurchaseItem.getQuantity() == null ||
                            requestPurchaseItem.getQuantity() < 1 ||
                                    requestPurchaseItem.getSupplierProduct() == null)){
                        continue;
                    }
                    i++;
                }
                if(requestPurchaseItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorPurchaseItemZero);
                }
                Set<String> serials = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestPurchaseItem requestPurchaseItem : requestPurchaseItemList){
                    if(!serials.add(requestPurchaseItem.getSupplierProduct())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorPurchaseDuplicateItem);
                    }
                }
                Purchase newPurchase = purchaseRepository.save(com.proyect.masterdata.domain.Purchase.builder()
                        .serial(requestPurchaseExcel.getSerial().toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .purchaseDocument(purchaseDocument)
                        .purchaseDocumentId(purchaseDocument.getId())
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .purchaseType(purchaseType)
                        .purchaseTypeId(purchaseType.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .build());
                int j = 0;
                for(Row row: sheet){
                    PurchaseItem purchaseItem = PurchaseItem.builder().build();
                    SupplierProduct supplierProduct;
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    int ji = 0;
                    for(Cell cell:row){
                        if(j>=2 && (cell.getCellType() == STRING) && (ji == 4)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            requestStockTransactionItem.setSupplierProductSerial(cell.getRichStringCellValue().getString().toUpperCase());
                            purchaseItem.setSupplierProduct(supplierProduct);
                            purchaseItem.setSupplierProductId(supplierProduct.getId());
                        }
                        if(j>=2 && (cell.getCellType() == NUMERIC) && (ji == 4)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int) (cell.getNumericCellValue())));
                            requestStockTransactionItem.setSupplierProductSerial(cell.getRichStringCellValue().getString().toUpperCase());
                            purchaseItem.setSupplierProduct(supplierProduct);
                            purchaseItem.setSupplierProductId(supplierProduct.getId());
                        }
                        if(j>=2 && (cell.getCellType()==NUMERIC)&&(ji == 5)){
                            if(cell.getNumericCellValue() > 0){
                                purchaseItem.setQuantity((int) cell.getNumericCellValue());
                                requestStockTransactionItem.setQuantity((int) cell.getNumericCellValue());
                            }

                        }
                        if(j>=2 && (cell.getCellType() == STRING) && (ji == 6)){
                            purchaseItem.setObservations(cell.getRichStringCellValue().getString().toUpperCase());
                        }

                        ji++;
                    }
                    if(j>=2 && (
                            purchaseItem.getQuantity() != null &&
                            purchaseItem.getQuantity() > 0 &&
                                    purchaseItem.getSupplierProduct() != null)){
                        purchaseItem.setPurchase(newPurchase);
                        purchaseItem.setPurchaseId(newPurchase.getId());
                        purchaseItem.setClient(user.getClient());
                        purchaseItem.setClientId(user.getClientId());
                        purchaseItem.setStatus(true);
                        purchaseItem.setTokenUser(user.getUsername());
                        stockTransactionItemList.add(requestStockTransactionItem);
                        purchaseItemRepository.save(purchaseItem);
                        iWarehouseStock.in(warehouse, purchaseItem.getSupplierProduct(), purchaseItem.getQuantity(),user);
                        iGeneralStock.in(purchaseItem.getSupplierProduct().getSerial(), purchaseItem.getQuantity(),user.getUsername());
                    }
                    if(j>=2 && (
                            purchaseItem.getQuantity() == null ||
                            purchaseItem.getQuantity() < 1 ||
                                    purchaseItem.getSupplierProduct() == null)){
                        continue;
                    }
                    j++;
                }
                iStockTransaction.save("S"+ requestPurchaseExcel.getSerial().toUpperCase(), warehouse,stockTransactionItemList,"COMPRA",user);
                iAudit.save("ADD_PURCHASE_EXCEL","COMPRA "+ newPurchase.getSerial()+" CREADA POR EXCEL.",newPurchase.getSerial(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            } catch (IOException | RuntimeException e) {
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> stockTransfer(RequestStockTransferExcel requestStockTransferExcel, MultipartFile multipartFile) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse originWarehouse;
            Warehouse destinationWarehouse;
            StockTransfer stockTransfer;
            List<RequestStockTransferItem> requestStockTransferItemList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestStockTransferExcel.getTokenUser().toUpperCase());
                originWarehouse = warehouseRepository.findByNameAndStatusTrue(requestStockTransferExcel.getOriginWarehouse().toUpperCase());
                destinationWarehouse = warehouseRepository.findByNameAndStatusTrue(requestStockTransferExcel.getDestinationWarehouse().toUpperCase());
                stockTransfer = stockTransferRepository.findBySerial(requestStockTransferExcel.getSerial().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(stockTransfer != null){
                throw new BadRequestExceptions(Constants.ErrorStockTransferExists);
            }

            if(originWarehouse == null){
                throw new BadRequestExceptions(Constants.ErrorOriginWarehouse);
            }

            if(destinationWarehouse == null){
                throw new BadRequestExceptions(Constants.ErrorDestinationWarehouse);
            }

            try{
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                int i = 0;
                for(Row row:sheet){
                    WarehouseStock originWarehouseStock = null;
                    SupplierProduct supplierProduct;
                    RequestStockTransferItem requestStockTransferItem = RequestStockTransferItem.builder().build();
                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && (ii==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestStockTransferItem.setSupplierProductSerial(supplierProduct.getSerial());
                            originWarehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(originWarehouse.getId(),supplierProduct.getId());
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestStockTransferItem.setSupplierProductSerial(supplierProduct.getSerial());
                            originWarehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(originWarehouse.getId(),supplierProduct.getId());
                        }
                        if((i>=1) && (cell.getCellType() == NUMERIC) && (ii==1) && (originWarehouseStock != null)){
                            if(((int) cell.getNumericCellValue())<1){
                                throw new BadRequestExceptions(Constants.ErrorStockTransferItemZero);
                            }
                            if(originWarehouseStock.getQuantity() < ((int) cell.getNumericCellValue())){
                                throw new BadRequestExceptions(Constants.ErrorOriginWarehouseStock);
                            }
                            requestStockTransferItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        ii++;
                    }
                    if(i>=1 && (
                            requestStockTransferItem.getQuantity() != null &&
                                    requestStockTransferItem.getSupplierProductSerial() != null)){
                        requestStockTransferItemList.add(requestStockTransferItem);
                    }
                    if(i>=1 && (
                            requestStockTransferItem.getQuantity() == null ||
                                    requestStockTransferItem.getSupplierProductSerial() == null)){
                        break;
                    }
                    i++;
                }
                if(requestStockTransferItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorStockTransferItemZero);
                }
                Set<String> skus = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestStockTransferItem requestStockTransferItem : requestStockTransferItemList){
                    if(!skus.add(requestStockTransferItem.getSupplierProductSerial())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorStockTransferDuplicateItem);
                    }
                }
                StockTransfer newStockTransfer = stockTransferRepository.save(StockTransfer.builder()
                        .serial(requestStockTransferExcel.getSerial().toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .originWarehouse(originWarehouse)
                        .originWarehouseId(originWarehouse.getId())
                        .destinationWarehouse(destinationWarehouse)
                        .destinationWarehouseId(destinationWarehouse.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .build());

                List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();
                int j = 0;
                for(Row row:sheet){
                    SupplierProduct supplierProduct;
                    StockTransferItem stockTransferItem = StockTransferItem.builder().build();
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    int ji = 0;
                    for(Cell cell:row){
                        if(j>=1 && (cell.getCellType() == STRING) && (ji==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            stockTransferItem.setSupplierProduct(supplierProduct);
                            stockTransferItem.setSupplierProductId(supplierProduct.getId());
                            requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && (ji==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int) (cell.getNumericCellValue())));
                            stockTransferItem.setSupplierProduct(supplierProduct);
                            stockTransferItem.setSupplierProductId(supplierProduct.getId());
                            requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && (ji==1)){
                            stockTransferItem.setQuantity((int) cell.getNumericCellValue());
                            requestStockTransactionItem.setQuantity(stockTransferItem.getQuantity());
                        }
                        ji++;
                    }
                    if(j>=1 && (
                            stockTransferItem.getQuantity() != null &&
                                    stockTransferItem.getSupplierProduct() != null)){
                        stockTransferItem.setStockTransfer(newStockTransfer);
                        stockTransferItem.setStockTransferId(newStockTransfer.getId());
                        stockTransferItem.setClient(user.getClient());
                        stockTransferItem.setClientId(user.getClientId());
                        stockTransferItem.setRegistrationDate(new Date(System.currentTimeMillis()));
                        stockTransferItem.setTokenUser(user.getUsername());
                        requestStockTransactionItemList.add(requestStockTransactionItem);
                        stockTransferItemRepository.save(stockTransferItem);
                        iWarehouseStock.out(newStockTransfer.getOriginWarehouse(),stockTransferItem.getSupplierProduct(),stockTransferItem.getQuantity(),user);
                        iWarehouseStock.in(newStockTransfer.getDestinationWarehouse(),stockTransferItem.getSupplierProduct(),stockTransferItem.getQuantity(),user);
                    }
                    if(j>=1 && (
                            stockTransferItem.getQuantity() == null ||
                                    stockTransferItem.getSupplierProduct() == null)){
                        break;
                    }
                    j++;
                }
                iStockTransaction.save("STO"+newStockTransfer.getId(),newStockTransfer.getOriginWarehouse(),requestStockTransactionItemList,"TRANSFERENCIA-SALIDA",user);
                iStockTransaction.save("STI"+newStockTransfer.getId(),newStockTransfer.getDestinationWarehouse(),requestStockTransactionItemList,"TRANSFERENCIA-ENTRADA",user);
                iAudit.save("ADD_STOCK_TRANSFER_EXCEL","TRANSFERENCIA DE STOCK "+newStockTransfer.getSerial()+" CREADA POR EXCEL.",newStockTransfer.getSerial(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> stockReturn(RequestStockReturnExcel requestStockReturnExcel, MultipartFile multipartFile) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            StockReturn stockReturn;
            Warehouse warehouse;
            Purchase purchase;
            List<RequestStockReturnItem> requestStockReturnItemList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestStockReturnExcel.getTokenUser().toUpperCase());
                stockReturn = stockReturnRepository.findBySerial(requestStockReturnExcel.getSerial());
                purchase = purchaseRepository.findByPurchaseTypeNameAndSerial( "EMBARQUE",requestStockReturnExcel.getPurchaseSerial());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(), requestStockReturnExcel.getWarehouse().toUpperCase());
            }

            if(stockReturn != null){
                throw new BadRequestExceptions(Constants.ErrorStockReturnExists);
            }

            if(purchase == null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            try {
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                int i = 0;
                for(Row row:sheet){
                    SupplierProduct supplierProduct = null;
                    PurchaseItem purchaseItem = null;
                    WarehouseStock warehouseStock;
                    RequestStockReturnItem requestStockReturnItem = RequestStockReturnItem.builder().build();
                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && (ii==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestStockReturnItem.setSupplierProductSerial(supplierProduct.getSerial());
                            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                            if(purchaseItem == null){
                                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                            }
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestStockReturnItem.setSupplierProductSerial(supplierProduct.getSerial());
                            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                            if(purchaseItem == null){
                                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                            }
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==1) && (supplierProduct != null)){
                            if(((int) cell.getNumericCellValue()) < 1){
                                throw new BadRequestExceptions(Constants.ErrorStockReturnItemZero);
                            }
                            warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),supplierProduct.getId());
                            if(((int) cell.getNumericCellValue()) > warehouseStock.getQuantity()){
                                throw new BadRequestExceptions(Constants.ErrorStockReturnWarehouseQuantity);
                            }
                            if(((int) cell.getNumericCellValue()) > purchaseItem.getQuantity()){
                                throw new BadRequestExceptions(Constants.ErrorStockReturnQuantity);
                            }
                            requestStockReturnItem.setQuantity(((int)cell.getNumericCellValue()));
                        }
                        if(i>=1&&(cell.getCellType() == STRING) && (ii==2)){
                            requestStockReturnItem.setObservations(cell.getRichStringCellValue().getString().toUpperCase());
                        }
                        ii++;
                    }
                    if(i>=1 && (
                            requestStockReturnItem.getQuantity() != null &&
                                    requestStockReturnItem.getObservations() != null &&
                                    requestStockReturnItem.getSupplierProductSerial() != null)){
                        requestStockReturnItemList.add(requestStockReturnItem);
                    }
                    if(i>=1 && (
                            requestStockReturnItem.getQuantity() == null &&
                                    requestStockReturnItem.getObservations() == null &&
                                    requestStockReturnItem.getSupplierProductSerial() == null)){
                        break;
                    }
                    i++;
                }
                if(requestStockReturnItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnItemZero);
                }
                Set<String> skus = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestStockReturnItem requestStockReturnItem : requestStockReturnItemList){
                    if(!skus.add(requestStockReturnItem.getSupplierProductSerial())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorStockReturnDuplicateItem);
                    }
                }
                StockReturn newStockReturn = stockReturnRepository.save(StockReturn.builder()
                        .serial(requestStockReturnExcel.getSerial().toUpperCase())
                                .purchase(purchase)
                                .purchaseId(purchase.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .status(true)
                        .build());
                List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();

                int j = 0;
                for(Row row:sheet){
                    int ji = 0;
                    SupplierProduct supplierProduct;
                    PurchaseItem purchaseItem;
                    StockReturnItem stockReturnItem = StockReturnItem.builder().build();
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    for(Cell cell:row){
                        if(j>=1 && (cell.getCellType() == STRING) && (ji==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                            requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
                            stockReturnItem.setSupplierProduct(supplierProduct);
                            stockReturnItem.setSupplierProductId(supplierProduct.getId());
                            stockReturnItem.setPurchaseItem(purchaseItem);
                            stockReturnItem.setPurchaseItemId(purchaseItem.getId());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && (ji==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                            requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
                            stockReturnItem.setSupplierProduct(supplierProduct);
                            stockReturnItem.setSupplierProductId(supplierProduct.getId());
                            stockReturnItem.setPurchaseItem(purchaseItem);
                            stockReturnItem.setPurchaseItemId(purchaseItem.getId());
                        }
                        if(j>=1&&(cell.getCellType()==NUMERIC)&&(ji==1)){
                            stockReturnItem.setQuantity((int) cell.getNumericCellValue());
                            requestStockTransactionItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        if(j>=1&&(cell.getCellType()==STRING)&&(ji==2)){
                            stockReturnItem.setObservations(cell.getRichStringCellValue().getString().toUpperCase());
                        }
                        ji++;
                    }
                    if(j>=1 && (
                            stockReturnItem.getQuantity() != null &&
                                    stockReturnItem.getSupplierProduct() != null &&
                            stockReturnItem.getObservations() != null)){
                        stockReturnItem.setPurchase(purchase);
                        stockReturnItem.setPurchaseId(purchase.getId());
                        stockReturnItem.setStockReturn(newStockReturn);
                        stockReturnItem.setStockReturnId(newStockReturn.getId());
                        stockReturnItem.setStatus(true);
                        stockReturnItem.setClient(user.getClient());
                        stockReturnItem.setClientId(user.getClientId());
                        stockReturnItem.setTokenUser(user.getUsername());
                        stockReturnItem.setRegistrationDate(new Date(System.currentTimeMillis()));
                        stockReturnItemRepository.save(stockReturnItem);
                        iWarehouseStock.out(warehouse,stockReturnItem.getSupplierProduct(),stockReturnItem.getQuantity(),user);
                        iGeneralStock.out(stockReturnItem.getSupplierProduct().getSerial(),stockReturnItem.getQuantity(),user.getUsername());
                        requestStockTransactionItemList.add(requestStockTransactionItem);
                    }
                    if(j>=1 && (
                            stockReturnItem.getQuantity() == null ||
                                    stockReturnItem.getSupplierProduct() == null ||
                            stockReturnItem.getObservations() == null)){
                        break;
                    }
                    j++;
                }

                iStockTransaction.save("SR"+newStockReturn.getId(), purchase.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-PROVEEDOR",user);
                iAudit.save("ADD_STOCK_RETURN_EXCEL","DEVOLUCION DE STOCK "+newStockReturn.getSerial()+" CREADA POR EXCEL.",newStockReturn.getSerial(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> stockReplenishment(Long orderId, MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            StockReplenishment stockReplenishment;
            Ordering ordering;
            List<RequestStockReplenishmentItem> requestStockReplenishmentItemList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                stockReplenishment = stockReplenishmentRepository.findByOrderId(orderId);
                ordering = orderingRepository.findById(orderId).orElse(null);
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(stockReplenishment != null){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentExists);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(!ordering.getOrderState().getName().equals("NO HAY STOCK")){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentOrderState);
            }

            try{
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                int i = 0;
                for(Row row : sheet){
                    int ii = 0;
                    Product product;
                    OrderItem orderItem = null;
                    RequestStockReplenishmentItem requestStockReplenishmentItem = RequestStockReplenishmentItem.builder().build();
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && (ii==0)){
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(product == null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            requestStockReplenishmentItem.setProductSku(product.getSku());
                            orderItem = orderItemRepository.findByOrderIdAndProductId(orderId,product.getId());
                            if(orderItem == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
                            }
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==0)){
                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int) (cell.getNumericCellValue())));
                            if(product == null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            requestStockReplenishmentItem.setProductSku(product.getSku());
                            orderItem = orderItemRepository.findByOrderIdAndProductId(orderId,product.getId());
                            if(orderItem == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
                            }
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==0)){
                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            if(product == null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            requestStockReplenishmentItem.setProductSku(product.getSku());
                            orderItem = orderItemRepository.findByOrderIdAndProductId(orderId,product.getId());
                            if(orderItem == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
                            }
                        }
                        if(i>=1 && (cell.getCellType()==NUMERIC)&&(ii==1)&&(orderItem != null)){
                            if(((int)cell.getNumericCellValue())<1){
                                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemZero);
                            }
                            if(((int)cell.getNumericCellValue())>orderItem.getQuantity()){
                                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentQuantity);
                            }
                            requestStockReplenishmentItem.setQuantity((int)cell.getNumericCellValue());
                        }
                        ii++;
                    }
                    if(i>=1 && (
                            requestStockReplenishmentItem.getQuantity() != null &&
                                    requestStockReplenishmentItem.getProductSku() != null)){
                        requestStockReplenishmentItemList.add(requestStockReplenishmentItem);
                    }
                    if(i>=1 && (
                            requestStockReplenishmentItem.getQuantity() == null ||
                                    requestStockReplenishmentItem.getProductSku() == null)){
                        break;
                    }
                    i++;
                }
                if(requestStockReplenishmentItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemZero);
                }
                Set<String> skus = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestStockReplenishmentItem requestStockReplenishmentItem : requestStockReplenishmentItemList){
                    if(!skus.add(requestStockReplenishmentItem.getProductSku())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorStockReplenishmentDuplicateItem);
                    }
                }
                StockReplenishment newStockReplenishment = stockReplenishmentRepository.save(StockReplenishment.builder()
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .build());
                int j = 0;
                for(Row row:sheet){
                    int ji = 0;
                    Product product;
                    StockReplenishmentItem stockReplenishmentItem = StockReplenishmentItem.builder().build();
                    for(Cell cell:row){
                        if(j>=1 && (cell.getCellType()==STRING)&&(ji==0)){
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            stockReplenishmentItem.setProduct(product);
                            stockReplenishmentItem.setProductId(product.getId());
                            stockReplenishmentItem.setOrdering(ordering);
                            stockReplenishmentItem.setOrderId(ordering.getId());
                        }
                        if(j>=1 && (cell.getCellType()==NUMERIC)&&(ji==0)){
                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            stockReplenishmentItem.setProduct(product);
                            stockReplenishmentItem.setProductId(product.getId());
                            stockReplenishmentItem.setOrdering(ordering);
                            stockReplenishmentItem.setOrderId(ordering.getId());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && (ji==1)){
                            stockReplenishmentItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        ji++;
                    }
                    if(j>=1 && (
                            stockReplenishmentItem.getProduct() != null &&
                                    stockReplenishmentItem.getQuantity() != null)){
                        stockReplenishmentItem.setClient(user.getClient());
                        stockReplenishmentItem.setClientId(user.getClientId());
                        stockReplenishmentItem.setRegistrationDate(new Date(System.currentTimeMillis()));
                        stockReplenishmentItem.setStatus(true);
                        stockReplenishmentItem.setTokenUser(user.getUsername());
                        stockReplenishmentItem.setStockReplenishment(newStockReplenishment);
                        stockReplenishmentItem.setStockReplenishmentId(newStockReplenishment.getId());
                        stockReplenishmentItemRepository.save(stockReplenishmentItem);
                    }
                    if(j>=1 && (
                            stockReplenishmentItem.getProduct() == null ||
                                    stockReplenishmentItem.getQuantity() == null)){
                        break;
                    }
                    j++;
                }
                iAudit.save("ADD_STOCK_REPLENISHMENT_EXCEL","RESTOCK "+newStockReplenishment.getOrderId()+" CREADO POR EXCEL.",newStockReplenishment.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException | IOException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> orderStock(Long orderId,String warehouseName, MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            Ordering ordering;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(warehouseName.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(warehouse == null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            try{
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                int i = 0;
                List<RequestOrderStockItem> requestOrderStockItemList = new ArrayList<>();
                for(Row row :sheet){
                    int ii = 0;
                    Product product;
                    OrderItem orderItem = null;
                    SupplierProduct supplierProduct;
                    RequestOrderStockItem requestOrderStockItem = RequestOrderStockItem.builder().build();
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && ii==0){
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(product == null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                            if(orderItem == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
                            }
                            requestOrderStockItem.setProduct(product.getSku());
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && ii==0){
                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            if(product == null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                            if(orderItem == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
                            }
                            requestOrderStockItem.setProduct(product.getSku());
                        }
                        if(i>=1 && (cell.getCellType() == STRING) && ii==1){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestOrderStockItem.setSupplierProduct(supplierProduct.getSerial());
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && ii==1){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestOrderStockItem.setSupplierProduct(supplierProduct.getSerial());
                        }
                        if(i>=1 && (cell.getCellType()==NUMERIC)&&(ii==2)&&(orderItem != null)){
                            if(((int) cell.getNumericCellValue()) < 1){
                                throw new BadRequestExceptions(Constants.ErrorOrderStockItemZero);
                            }
                            if(((int) cell.getNumericCellValue()) > orderItem.getQuantity()){
                                throw new BadRequestExceptions(Constants.ErrorOrderStockItemQuantity);
                            }
                            requestOrderStockItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        ii++;
                    }
                    if(i>=1 && (
                            requestOrderStockItem.getQuantity() != null &&
                                    requestOrderStockItem.getProduct() != null &&
                                    requestOrderStockItem.getSupplierProduct() != null)){
                        requestOrderStockItemList.add(requestOrderStockItem);
                    }
                    if(i>=1 && (
                            requestOrderStockItem.getQuantity() == null ||
                                    requestOrderStockItem.getProduct() == null ||
                                    requestOrderStockItem.getSupplierProduct() == null)){
                        break;
                    }
                    i++;
                }
                if(requestOrderStockItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorOrderStockItemZero);
                }
                Set<String> serials = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                    if(!serials.add(requestOrderStockItem.getSupplierProduct())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockDuplicateItem);
                    }
                    Boolean existsStock = iOrderStockItem.checkWarehouseItemStock(ordering.getId(),warehouse,requestOrderStockItem).get();
                    if(!existsStock){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockQuantity);
                    }
                }
                Map<String,Integer> checkCount = requestOrderStockItemList.stream().collect(
                        Collectors.groupingBy(
                                RequestOrderStockItem::getProduct,
                                Collectors.summingInt(RequestOrderStockItem::getQuantity)
                        )
                );
                checkCount.forEach((key,value)->{
                    Product product = productRepository.findBySkuAndStatusTrue(key);
                    OrderItem orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                    if(value > orderItem.getQuantity()){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockProductQuantity);
                    }
                });
                OrderStock orderStock = orderStockRepository.save(OrderStock.builder()
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .status(true)
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .build());
                int j = 0;
                for(Row row :sheet){
                    int ji = 0;
                    Product product;
                    OrderItem orderItem;
                    SupplierProduct supplierProduct;
                    OrderStockItem orderStockItem = OrderStockItem.builder().build();
                    for(Cell cell:row){
                        if(j>=1 && (cell.getCellType() == STRING) && ji==0){
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                            orderStockItem.setOrderItem(orderItem);
                            orderStockItem.setOrderItemId(orderItem.getId());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && ji==0){
                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                            orderStockItem.setOrderItem(orderItem);
                            orderStockItem.setOrderItemId(orderItem.getId());
                        }
                        if(j>=1 && (cell.getCellType() == STRING) && ji==1){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getStringCellValue().toUpperCase());
                            orderStockItem.setSupplierProduct(supplierProduct);
                            orderStockItem.setSupplierProductId(supplierProduct.getId());
                        }
                        if(j>=1 && (cell.getCellType()==NUMERIC)&&ji==2){
                            orderStockItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        ji++;
                    }
                    if(j>=1 && (
                            orderStockItem.getOrderItem() != null &&
                            orderStockItem.getQuantity() != null &&
                            orderStockItem.getSupplierProduct() != null)){
                        orderStockItem.setOrdering(ordering);
                        orderStockItem.setOrderId(ordering.getId());
                        orderStockItem.setStatus(true);
                        orderStockItem.setRegistrationDate(new Date(System.currentTimeMillis()));
                        orderStockItem.setClient(user.getClient());
                        orderStockItem.setClientId(user.getClientId());
                        orderStockItem.setTokenUser(user.getUsername());
                        orderStockItem.setOrderStock(orderStock);
                        orderStockItem.setOrderStockId(orderStock.getId());
                        orderStockItemRepository.save(orderStockItem);
                    }
                    if(j>=1 && (
                            orderStockItem.getOrderItem() == null ||
                                    orderStockItem.getQuantity() == null ||
                                    orderStockItem.getSupplierProduct() == null
                            )){
                        break;
                    }
                    j++;
                }
                iAudit.save("ADD_ORDER_STOCK_EXCEL","PREPARACION DEL PEDIDO #"+orderStock.getOrderId()+" CREADO POR EXCEL.",orderStock.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            } catch (ExecutionException | InterruptedException | IOException | RuntimeException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> orderReturn(Long orderId, MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderReturn orderReturn;
            OrderStock orderStock;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                orderReturn = orderReturnRepository.findByOrderId(orderId);
                orderStock = orderStockRepository.findByOrderIdAndClientId(orderId,user.getClientId());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderReturn != null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnExists);
            }
            if(orderStock == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }
            try {
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                List<RequestOrderReturnItem> requestOrderReturnItemList = new ArrayList<>();
                int i = 0;
                for(Row row:sheet){
                    SupplierProduct supplierProduct;
                    Product product;
                    OrderStockItem orderStockItem = null;
                    OrderReturnType orderReturnType;
                    RequestOrderReturnItem requestOrderReturnItem = RequestOrderReturnItem.builder().build();
                    int ii = 0;
                    for(Cell cell:row){
                        if((i>=1) && (cell.getCellType() == STRING) && (ii==0)){
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(product==null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            requestOrderReturnItem.setProductSku(product.getSku());
                        }
                        if((i>=1) && (cell.getCellType() == NUMERIC) && (ii==0)){
                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            if(product==null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            requestOrderReturnItem.setProductSku(product.getSku());
                        }
                        if(i>=1 && (cell.getCellType() == STRING) && (ii==1)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct==null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
                            if(orderStockItem == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderStockItem);
                            }
                            requestOrderReturnItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==1)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int) (cell.getNumericCellValue())));
                            if(supplierProduct==null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
                            if(orderStockItem == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderStockItem);
                            }
                            requestOrderReturnItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        if(i>=1 && (cell.getCellType()==NUMERIC)&&(ii==2) && (orderStockItem != null)){
                            if(((int) cell.getNumericCellValue())<1){
                                throw new BadRequestExceptions(Constants.ErrorOrderReturnItemZero);
                            }
                            if(orderStockItem.getQuantity()<((int) cell.getNumericCellValue())){
                                throw new BadRequestExceptions(Constants.ErrorOrderReturnItemQuantity);
                            }
                            requestOrderReturnItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        if(i>=1 && (cell.getCellType()==STRING)&&(ii==3)){
                            orderReturnType = orderReturnTypeRepository.findByNameAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(orderReturnType == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderReturnType);
                            }
                            requestOrderReturnItem.setOrderReturnType(orderReturnType.getName());
                        }
                        ii++;
                    }
                    if(i>=1 && (
                            requestOrderReturnItem.getQuantity()!=null &&
                                    requestOrderReturnItem.getOrderReturnType() !=null &&
                                    requestOrderReturnItem.getProductSku() != null &&
                                    requestOrderReturnItem.getSupplierProductSerial() != null)){
                        requestOrderReturnItemList.add(requestOrderReturnItem);
                    }
                    if(i>=1 && (
                            requestOrderReturnItem.getQuantity()==null ||
                                    requestOrderReturnItem.getOrderReturnType() == null ||
                                    requestOrderReturnItem.getProductSku() == null ||
                                    requestOrderReturnItem.getSupplierProductSerial() == null)){
                        break;
                    }
                    i++;
                }
                if(requestOrderReturnItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorOrderReturnItemZero);
                }
                Set<String> serials = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestOrderReturnItem requestOrderReturnItem : requestOrderReturnItemList){
                    if(!serials.add(requestOrderReturnItem.getSupplierProductSerial())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockDuplicateItem);
                    }
                }
                Map<String,Integer> checkCount = requestOrderReturnItemList.stream().collect(
                        Collectors.groupingBy(
                                    RequestOrderReturnItem::getSupplierProductSerial,
                                    Collectors.summingInt(RequestOrderReturnItem::getQuantity)
                        )
                );
                checkCount.forEach((key,value)->{
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(key);
                    OrderStockItem orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
                    if(value > orderStockItem.getQuantity()){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockProductQuantity);
                    }
                });
                OrderReturn newOrderReturn = orderReturnRepository.save(OrderReturn.builder()
                        .order(orderStock.getOrdering())
                        .orderId(orderStock.getOrderId())
                        .orderStock(orderStock)
                        .orderStockId(orderStock.getId())
                        .tokenUser(user.getUsername())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .status(true)
                        .build());
                List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();
                int j = 0;
                for(Row row:sheet){
                    int ji = 0;
                    OrderReturnItem orderReturnItem = OrderReturnItem.builder().build();
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    for(Cell cell:row){
                        Product product;
                        SupplierProduct supplierProduct;
                        OrderReturnType orderReturnType;
                        if(j>=1 && (cell.getCellType() == STRING) && (ji==0)){
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().substring(1).toUpperCase());
                            orderReturnItem.setProduct(product);
                            orderReturnItem.setProductId(product.getId());
                        }
                        if(j>=1 && (cell.getCellType() == STRING) && (ji==1)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getStringCellValue().toUpperCase());
                            orderReturnItem.setSupplierProduct(supplierProduct);
                            orderReturnItem.setSupplierProductId(supplierProduct.getId());
                            requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && (ji==2)){
                            orderReturnItem.setQuantity((int) cell.getNumericCellValue());
                            requestStockTransactionItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        if(j>=1 && (cell.getCellType() == STRING) && (ji==3)){
                            orderReturnType = orderReturnTypeRepository.findByNameAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            orderReturnItem.setOrderReturnType(orderReturnType);
                            orderReturnItem.setOrderReturnTypeId(orderReturnType.getId());
                        }
                        ji++;
                    }
                    if(j>=1 && (
                            orderReturnItem.getProduct() != null &&
                                    orderReturnItem.getQuantity() != null &&
                                    orderReturnItem.getSupplierProduct() != null &&
                                    orderReturnItem.getOrderReturnType() != null
                    )){
                        orderReturnItem.setStatus(true);
                        orderReturnItem.setRegistrationDate(new Date(System.currentTimeMillis()));
                        orderReturnItem.setClient(user.getClient());
                        orderReturnItem.setClientId(user.getClientId());
                        orderReturnItem.setOrderReturn(newOrderReturn);
                        orderReturnItem.setOrderReturnId(newOrderReturn.getId());
                        orderReturnItem.setTokenUser(user.getUsername());
                        orderReturnItemRepository.save(orderReturnItem);
                        iGeneralStock.in(orderReturnItem.getSupplierProduct().getSerial(),orderReturnItem.getQuantity(),user.getUsername());
                        iWarehouseStock.in(orderStock.getWarehouse(),orderReturnItem.getSupplierProduct(),orderReturnItem.getQuantity(),user);
                        requestStockTransactionItemList.add(requestStockTransactionItem);
                    }
                    if(j>=1 && (
                            orderReturnItem.getProduct() == null ||
                                    orderReturnItem.getQuantity() == null ||
                                    orderReturnItem.getSupplierProduct() == null ||
                                    orderReturnItem.getOrderReturnType() == null
                    )){
                        break;
                    }
                    j++;
                }
                iStockTransaction.save("OR"+orderStock.getOrdering().getId(),orderStock.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-COMPRADOR",user);
                iAudit.save("ADD_ORDER_RETURN_EXCEL","DEVOLUCION DE PEDIDO "+newOrderReturn.getOrderId()+" CREADA POR EXCEL.",newOrderReturn.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> product(MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;

            try {
                user = userRepository
                        .findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            try {
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                List<Product> products = new ArrayList<>();
                List<ProductPrice> productPrices = new ArrayList<>();
                Set<String> skus = new HashSet<>();
                boolean hasDuplicate = false;
                int i = 0;
                for(Row row:sheet){
                    Product product;
                    Model model;
                    Color color;
                    CategoryProduct categoryProduct=null;
                    Size size;
                    UnitType unitType=null;
                    Unit unit;
                    Product newProduct = Product.builder().build();
                    ProductPrice productPrice = ProductPrice.builder().build();
                    for(int ii = 0;ii <= 9;ii++){
                        if((i>=1)&&(
                                (row.getCell(0)==null) ||
                                        (row.getCell(1)==null)) ||
                                (row.getCell(2)==null) ||
                                (row.getCell(3)==null) ||
                                (row.getCell(4)==null) ||
                                (row.getCell(5)==null) ||
                                (row.getCell(6)==null) ||
                                (row.getCell(7)==null) ||
                                (row.getCell(9)==null)
                        ){
                            break;
                        }
                        if((i>=1) && (row.getCell(0).getCellType() == STRING) && (ii==0)){
                            product = productRepository.findBySku(row.getCell(0).getRichStringCellValue().getString().toUpperCase());
                            if(product != null){
                                throw new BadRequestExceptions(Constants.ErrorProductExists);
                            }
                            newProduct.setSku(row.getCell(0).getRichStringCellValue().getString().toUpperCase());
                        }
                        if((i>=1) && (row.getCell(0).getCellType() == NUMERIC) && (ii==0)){
                            product = productRepository.findBySku(String.valueOf((int) (row.getCell(0).getNumericCellValue())));
                            if(product != null){
                                throw new BadRequestExceptions(Constants.ErrorProductExists);
                            }
                            newProduct.setSku(row.getCell(0).getRichStringCellValue().getString().toUpperCase());
                        }
                        if((i>=1)&&(row.getCell(2).getCellType() == STRING) && (ii==2)){
                            model = modelRepository.findByNameAndClientIdAndStatusTrue(row.getCell(2).getRichStringCellValue().getString().toUpperCase(),user.getClientId());
                            if(model == null){
                                throw new BadRequestExceptions(Constants.ErrorModel);
                            }
                            newProduct.setModel(model);
                            newProduct.setModelId(model.getId());
                        }
                        if((i>=1)&&(row.getCell(3).getCellType()==STRING)&&(ii==3)){
                            color = colorRepository.findByNameAndStatusTrue(row.getCell(3).getRichStringCellValue().getString().toUpperCase());
                            if(color==null){
                                throw new BadRequestExceptions(Constants.ErrorColor);
                            }
                            newProduct.setColor(color);
                            newProduct.setColorId(color.getId());
                        }
                        if((i>=1)&&(row.getCell(4).getCellType()==STRING)&&(ii==4)){
                            categoryProduct = categoryProductRepository.findByNameAndStatusTrue(row.getCell(4).getRichStringCellValue().getString().toUpperCase());
                            if(categoryProduct==null){
                                throw new BadRequestExceptions(Constants.ErrorCategory);
                            }
                            newProduct.setCategoryProduct(categoryProduct);
                            newProduct.setCategoryProductId(categoryProduct.getId());
                        }
                        if((i>=1)&&(row.getCell(5).getCellType()==STRING)&&(ii==5)&&(categoryProduct!=null)){
                            size = sizeRepository.findByNameAndStatusTrue(row.getCell(5).getRichStringCellValue().getString().toUpperCase());
                            if(size==null){
                                throw new BadRequestExceptions(Constants.ErrorSize);
                            }

                            if(!Objects.equals(size.getSizeTypeId(), categoryProduct.getSizeTypeId())){
                                throw new BadRequestExceptions(Constants.ErrorSizeTypeCategoryProduct);
                            }
                            newProduct.setSize(size);
                            newProduct.setSizeId(size.getId());
                        }
                        if((i>=1)&&(row.getCell(5).getCellType()==NUMERIC)&&(ii==5)&&(categoryProduct!=null)){
                            size = sizeRepository.findByNameAndStatusTrue(String.valueOf((int) row.getCell(5).getNumericCellValue()));
                            if(size==null){
                                throw new BadRequestExceptions(Constants.ErrorSize);
                            }
                            if(!Objects.equals(size.getSizeTypeId(), categoryProduct.getSizeTypeId())){
                                throw new BadRequestExceptions(Constants.ErrorSizeTypeCategoryProduct);
                            }
                            newProduct.setSize(size);
                            newProduct.setSizeId(size.getId());
                        }
                        if((i>=1)&&(row.getCell(6).getCellType()==STRING)&&(ii==6)){
                            unitType = unitTypeRepository.findByNameAndStatusTrue(row.getCell(6).getRichStringCellValue().getString().toUpperCase());
                            if(unitType == null){
                                throw new BadRequestExceptions(Constants.ErrorUnitType);
                            }
                        }
                        if((i>=1)&&(row.getCell(7).getCellType()==STRING)&&(ii==7)){
                            assert unitType != null;
                            unit = unitRepository.findByNameAndUnitTypeIdAndStatusTrue(row.getCell(7).getRichStringCellValue().getString().toUpperCase(),unitType.getId());
                            if(unit==null){
                                throw new BadRequestExceptions(Constants.ErrorUnit);
                            }
                            newProduct.setUnit(unit);
                            newProduct.setUnitId(unit.getId());
                        }
                        if((i>=1)&&(row.getCell(8)!=null)&&(row.getCell(8).getCellType()==STRING)&&(ii==8)){
                            newProduct.setCharacteristics(row.getCell(8).getRichStringCellValue().getString().toUpperCase());
                        }
                        if(newProduct.getCharacteristics()==null){
                            newProduct.setCharacteristics("NO APLICA");
                        }
                        if((i>=1)&&(row.getCell(9).getCellType()==NUMERIC)&&(ii==9)){
                            if(row.getCell(9).getNumericCellValue() < 0.01){
                                throw new BadRequestExceptions(Constants.ErrorProductPriceZero);
                            }
                            productPrice.setUnitSalePrice(row.getCell(9).getNumericCellValue());
                        }
                    }
                    if(i>=1 && (
                            newProduct.getSku() != null &&
                                    newProduct.getSize() != null &&
                                    newProduct.getUnit() != null &&
                                    newProduct.getCategoryProduct() != null &&
                                    newProduct.getModel() != null &&
                                    productPrice.getUnitSalePrice() != null &&
                                    newProduct.getColor() != null
                            )){
                        newProduct.setStatus(true);
                        newProduct.setRegistrationDate(new Date(System.currentTimeMillis()));
                        newProduct.setUpdateDate(new Date(System.currentTimeMillis()));
                        newProduct.setTokenUser(user.getUsername());
                        newProduct.setClient(user.getClient());
                        newProduct.setClientId(user.getClientId());
                        productPrice.setTokenUser(user.getUsername());
                        productPrice.setRegistrationDate(new Date(System.currentTimeMillis()));
                        productPrice.setUpdateDate(new Date(System.currentTimeMillis()));
                        productPrice.setStatus(true);
                        products.add(newProduct);
                        productPrices.add(productPrice);
                    }
                    if(i>=1 && (
                            newProduct.getSku() == null ||
                                    newProduct.getSize() == null ||
                                    newProduct.getUnit() == null ||
                                    productPrice.getUnitSalePrice() == null ||
                                    newProduct.getModel() == null ||
                                    newProduct.getCategoryProduct() == null ||
                                    newProduct.getColor() == null
                    )){
                        break;
                    }
                    i++;
                }

                if(products.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorProduct);
                }

                for(int j = 0;j < products.size();j++){
                    if(products.size() != productPrices.size()){
                        throw new IllegalArgumentException("Both lists must have the same size");
                    }
                    Product product = products.get(j);
                    ProductPrice productPrice = productPrices.get(j);
                    if(!skus.add(product.getSku())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorProductExists);
                    }else {
                        Product storedProduct = productRepository.save(product);
                        productPrice.setProductId(storedProduct.getId());
                        productPrice.setProduct(storedProduct);
                        productPriceRepository.save(productPrice);
                        iAudit.save("ADD_PRODUCT_EXCEL","PRODUCTO DE MARKETING "+product.getSku()+" CREADO POR EXCEL.",product.getSku(),user.getUsername());
                    }
                }
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException | IOException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> supplierProduct(MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            try {
                user = userRepository
                        .findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            try {
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                List<SupplierProduct> supplierProducts = new ArrayList<>();
                Set<String> skus = new HashSet<>();
                boolean hasDuplicate = false;
                int i = 0;
                for(Row row:sheet){
                    int ii = 0;
                    Product product;
                    SupplierProduct supplierProduct;
                    Supplier supplier;
                    SupplierProduct newSupplierProduct = SupplierProduct.builder().build();
                    for(Cell cell:row){
                        if((i>=1) && (cell.getCellType() == STRING) && (ii==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getStringCellValue().toUpperCase());
                            if(supplierProduct!=null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierExists);
                            }
                            newSupplierProduct.setSerial(cell.getStringCellValue().toUpperCase());
                        }
                        if((i>=1) && (cell.getCellType() == NUMERIC) && (ii==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            if(supplierProduct!=null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierExists);
                            }
                            newSupplierProduct.setSerial(String.valueOf((int)(cell.getNumericCellValue())));
                        }
                        if((i>=1) && (cell.getCellType() == STRING) && (ii==1)){
                            product = productRepository.findBySkuAndStatusTrue(cell.getStringCellValue().toUpperCase());
                            if(product == null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            newSupplierProduct.setProduct(product);
                            newSupplierProduct.setProductId(product.getId());
                        }
                        if((i>=1) && (cell.getCellType() == STRING) && (ii==2)){
                            supplier = supplierRepository.findByBusinessNameAndClientId(cell.getStringCellValue().toUpperCase(),user.getClientId());
                            if(supplier == null){
                                throw new BadRequestExceptions(Constants.ErrorUser);
                            }
                            newSupplierProduct.setSupplier(supplier);
                            newSupplierProduct.setSupplierId(supplier.getId());
                        }
                        if((i>=1) && (cell.getCellType() == NUMERIC) && (ii==3)){
                            if(cell.getNumericCellValue() < 0.01){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProductZero);
                            }
                            newSupplierProduct.setPurchasePrice(cell.getNumericCellValue());
                        }
                        ii++;
                    }
                    if(i>=1 && (
                            newSupplierProduct.getSerial() != null &&
                                    newSupplierProduct.getProduct() != null &&
                                    newSupplierProduct.getSupplier() != null &&
                                    newSupplierProduct.getPurchasePrice() != null
                            )){
                        newSupplierProduct.setStatus(true);
                        newSupplierProduct.setTokenUser(user.getUsername());
                        newSupplierProduct.setRegistrationDate(new Date(System.currentTimeMillis()));
                        newSupplierProduct.setUpdateDate(new Date(System.currentTimeMillis()));
                        newSupplierProduct.setClient(user.getClient());
                        newSupplierProduct.setClientId(user.getClientId());
                        supplierProducts.add(newSupplierProduct);
                    }
                    if(i>=1 && (
                            newSupplierProduct.getSerial() == null ||
                                    newSupplierProduct.getProduct() == null ||
                                    newSupplierProduct.getSupplier() == null ||
                                    newSupplierProduct.getPurchasePrice() == null
                    )){
                        break;
                    }
                    i++;
                }
                if(supplierProducts.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }
                for(SupplierProduct supplierProduct : supplierProducts){
                    if(!skus.add(supplierProduct.getSerial())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorSupplierProductExists);
                    }else{
                        supplierProductRepository.save(supplierProduct);
                        iAudit.save("ADD_SUPPLIER_PRODUCT_EXCEL","PRODUCTO DE INVENTARIO "+supplierProduct.getSerial()+" CREADO POR EXCEL.",supplierProduct.getSerial(),user.getUsername());
                    }
                }
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public String getExcelColumnReference(Character character,Integer index) {
        StringBuilder column = new StringBuilder();
        while(index >= 0){
            if(character == 'A'){
                column.insert(0,(char) ('A'+(index % 26)));
                index = (index/26) - 1;
            }
            if(character == 'B'){
                column.insert(0,(char) ('B'+(index % 25)));
                index = (index/25) - 1;
            }
        }
        return column.toString();
    }

    @Override
    public CompletableFuture<ResponseSuccess> model(MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            try {
                user = userRepository
                        .findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            try{
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);

                List<Model> models = new ArrayList<>();
                Set<String> modelNames = new HashSet<>();
                boolean hasDuplicate = false;
                int i = 0;
                for(Row row:sheet){
                    int ii = 0;
                    Brand brand;
                    Model model;
                    Model newModel = Model.builder().build();
                    for(Cell cell:row){
                        if((i>=1) && (cell.getCellType() == STRING) && (ii==0)){
                            brand = brandRepository.findByNameAndClientIdAndStatusTrue(cell.getStringCellValue().toUpperCase(),user.getClientId());
                            if(brand==null){
                                throw new BadRequestExceptions(Constants.ErrorBrand);
                            }
                            newModel.setBrand(brand);
                            newModel.setBrandId(brand.getId());
                        }
                        if((i>=1)&&(cell.getCellType() == STRING) && (ii==1)){
                            model = modelRepository.findByNameAndClientId(cell.getRichStringCellValue().getString().toUpperCase(),user.getClientId());
                            if(model!=null){
                                throw new BadRequestExceptions(Constants.ErrorModelExists);
                            }
                            newModel.setName(cell.getRichStringCellValue().getString().toUpperCase());
                        }
                        if((i>=1)&&(cell.getCellType() == STRING) && (ii==2)){
                            model = modelRepository.findBySkuAndClientId(cell.getRichStringCellValue().getString().toUpperCase(),user.getClientId());
                            if(model!=null){
                                throw new BadRequestExceptions(Constants.ErrorModelExists);
                            }
                            newModel.setSku(cell.getRichStringCellValue().getString().toUpperCase());
                        }
                        ii++;
                    }
                    if(i>=1 && (
                            newModel.getName() != null &&
                                    newModel.getBrand() != null &&
                                    newModel.getSku() != null
                    )){
                        newModel.setStatus(true);
                        newModel.setRegistrationDate(new Date(System.currentTimeMillis()));
                        newModel.setUpdateDate(new Date(System.currentTimeMillis()));
                        newModel.setTokenUser(user.getUsername());
                        newModel.setClient(user.getClient());
                        newModel.setClientId(user.getClientId());
                        models.add(newModel);
                    }
                    if(i>=1 && (
                            newModel.getName() == null ||
                                    newModel.getBrand() == null ||
                                    newModel.getSku() == null
                    )){
                        break;
                    }
                    i++;
                }
                if(models.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorModel);
                }
                for(Model model : models){
                    if(!modelNames.add(model.getSku())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorModelExists);
                    }else{
                        modelRepository.save(model);
                        iAudit.save("ADD_MODEL_EXCEL","MODEL "+model.getSku()+" CREADO POR EXCEL.",model.getSku(),user.getUsername());
                    }
                }
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException | IOException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
