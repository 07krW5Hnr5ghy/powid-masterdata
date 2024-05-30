package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
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
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
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
    private final PurchaseRepository purchaseRepository;
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    private final SupplierRepository supplierRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final ShipmentRepository shipmentRepository;
    private final ShipmentTypeRepository shipmentTypeRepository;
    private final WarehouseRepository warehouseRepository;
    private final StockReturnRepository stockReturnRepository;
    private final ShipmentItemRepository shipmentItemRepository;
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
    @Override
    public CompletableFuture<ResponseSuccess> purchase(RequestPurchaseExcel requestPurchaseExcel,MultipartFile multipartFile) throws BadRequestExceptions {
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
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                int i = 0;
                List<RequestPurchaseItem> requestPurchaseItemList = new ArrayList<>();
                for(Row row:sheet){
                    SupplierProduct supplierProduct;
                    RequestPurchaseItem requestPurchaseItem = RequestPurchaseItem.builder().build();
                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType()==STRING) && (ii==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestPurchaseItem.setSupplierProductSerial(supplierProduct.getSerial());
                            System.out.println(cell);
                        }
                        if(i>=1 && (cell.getCellType()==NUMERIC)&&(ii==1)){
                            if(((int)cell.getNumericCellValue())<1){
                                throw new BadRequestExceptions(Constants.ErrorPurchaseItemZero);
                            }
                        }
                        ii++;
                    }
                    requestPurchaseItemList.add(requestPurchaseItem);
                    i++;
                }
                Set<String> serials = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestPurchaseItem requestPurchaseItem : requestPurchaseItemList){
                    if(!serials.add(requestPurchaseItem.getSupplierProductSerial())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorPurchaseDuplicateItem);
                    }
                }
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
                int j = 0;
                for(Row row : sheet){
                    PurchaseItem purchaseItem = PurchaseItem.builder().build();
                    SupplierProduct supplierProduct;
                    purchaseItem.setPurchase(newPurchase);
                    purchaseItem.setPurchaseId(newPurchase.getId());
                    for(Cell cell:row){
                        if(j>=1 && (cell.getCellType()==STRING)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            purchaseItem.setSupplierProduct(supplierProduct);
                            purchaseItem.setSupplierProductId(supplierProduct.getId());
                        }

                        if(j>=1 && (cell.getCellType()==NUMERIC)){
                            System.out.println("quantity");
                            purchaseItem.setQuantity((int) cell.getNumericCellValue());
                            if(purchaseItem.getQuantity() == null){
                                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                            }
                        }

                    }
                    if(j>=1){
                        purchaseItem.setStatus(true);
                        purchaseItem.setClient(user.getClient());
                        purchaseItem.setClientId(user.getClientId());
                        purchaseItem.setRegistrationDate(new Date(System.currentTimeMillis()));
                        purchaseItem.setTokenUser(user.getUsername());
                        purchaseItemRepository.save(purchaseItem);
                    }
                    j++;
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

    @Override
    public CompletableFuture<ResponseSuccess> shipment(RequestShipmentExcel requestShipmentExcel, MultipartFile multipartFile) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            Shipment shipment;
            Purchase purchase;
            ShipmentType shipmentType;
            StockReturn stockReturn;
            try {
                user = userRepository.findByUsernameAndStatusTrue(requestShipmentExcel.getTokenUser().toUpperCase());
                shipmentType = shipmentTypeRepository.findByNameAndStatusTrue(requestShipmentExcel.getShipmentType().toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestShipmentExcel.getWarehouse().toUpperCase());
                shipment = shipmentRepository.findByPurchaseSerialAndShipmentTypeId(requestShipmentExcel.getPurchaseSerial().toUpperCase(),shipmentType.getId());
                purchase = purchaseRepository.findBySerialAndStatusTrue(requestShipmentExcel.getPurchaseSerial().toUpperCase());
            } catch (RuntimeException e) {
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

            if (shipment != null) {
                throw new BadRequestExceptions(Constants.ErrorShipmentExists);
            }

            if(purchase == null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(shipmentType == null){
                throw new BadRequestExceptions(Constants.ErrorShipmentType);
            }

            try {

                if(Objects.equals(shipmentType.getName(), "DEVOLUCION")){
                    stockReturn = stockReturnRepository.findBySerial(requestShipmentExcel.getPurchaseSerial());
                    if(stockReturn == null){
                        throw new BadRequestExceptions(Constants.ErrorShipmentReturn);
                    }
                }

                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                int i = 0;
                List<RequestStockTransactionItem> stockTransactionItemList = new ArrayList<>();
                List<RequestShipmentItem> requestShipmentItemList = new ArrayList<>();
                for(Row row:sheet){
                    SupplierProduct supplierProduct;
                    PurchaseItem purchaseItem;
                    RequestShipmentItem requestShipmentItem = RequestShipmentItem.builder().build();
                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && (ii == 0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestShipmentItem.setSupplierProductSerial(supplierProduct.getSerial());
                            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                            if(purchaseItem == null){
                                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                            }
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==1)){
                            if(((int) cell.getNumericCellValue()) < 1){
                                throw new BadRequestExceptions(Constants.ErrorShipmentItemZero);
                            }
                        }
                        ii++;
                    }
                    requestShipmentItemList.add(requestShipmentItem);
                    i++;
                }
                Set<String> serials = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestShipmentItem requestShipmentItem : requestShipmentItemList){
                    if(!serials.add(requestShipmentItem.getSupplierProductSerial())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorShipmentDuplicateItem);
                    }
                }
                Shipment newShipment = shipmentRepository.save(Shipment.builder()
                        .purchaseSerial(requestShipmentExcel.getPurchaseSerial().toUpperCase())
                        .status(true)
                        .purchase(purchase)
                        .purchaseId(purchase.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .shipmentType(shipmentType)
                        .shipmentTypeId(shipmentType.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .build());
                int j = 0;
                for(Row row: sheet){
                    ShipmentItem shipmentItem = ShipmentItem.builder().build();
                    SupplierProduct supplierProduct;
                    PurchaseItem purchaseItem;
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    int ji = 0;
                    for(Cell cell:row){
                        System.out.println(ji);
                        if(j>=1 && (cell.getCellType() == STRING) && (ji == 0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                            requestStockTransactionItem.setSupplierProductSerial(cell.getRichStringCellValue().getString().toUpperCase());
                            shipmentItem.setSupplierProduct(supplierProduct);
                            shipmentItem.setSupplierProductId(supplierProduct.getId());
                            shipmentItem.setPurchaseItem(purchaseItem);
                            shipmentItem.setPurchaseItemId(purchaseItem.getId());
                        }
                        if(j>=1 && (cell.getCellType()==NUMERIC)){
                            shipmentItem.setQuantity((int) cell.getNumericCellValue());
                            requestStockTransactionItem.setQuantity((int) cell.getNumericCellValue());

                        }
                        if(j>=1 && (cell.getCellType() == STRING) && (ji == 2)){
                            shipmentItem.setObservations(cell.getRichStringCellValue().getString().toUpperCase());
                        }

                        ji++;
                    }
                    if(j>=1){
                        shipmentItem.setShipment(newShipment);
                        shipmentItem.setShipmentId(newShipment.getId());
                        shipmentItem.setClient(user.getClient());
                        shipmentItem.setClientId(user.getClientId());
                        shipmentItem.setStatus(true);
                        shipmentItem.setTokenUser(user.getUsername());
                        stockTransactionItemList.add(requestStockTransactionItem);
                        shipmentItemRepository.save(shipmentItem);
                        iWarehouseStock.in(warehouse,shipmentItem.getSupplierProduct(),shipmentItem.getQuantity(),user);
                        iGeneralStock.in(shipmentItem.getSupplierProduct().getSerial(),shipmentItem.getQuantity(),user.getUsername());
                    }
                    j++;
                }
                StockTransaction newStockTransaction = iStockTransaction.save("S"+requestShipmentExcel.getPurchaseSerial().toUpperCase(), warehouse,stockTransactionItemList,"ENTRADA",user);
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }catch (RuntimeException e){
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
                        if((i>=1) && (cell.getCellType() == NUMERIC) && (ii==1) && (originWarehouseStock != null)){
                            if(((int) cell.getNumericCellValue())<1){
                                throw new BadRequestExceptions(Constants.ErrorStockTransferItemZero);
                            }
                            if(originWarehouseStock.getQuantity() < ((int) cell.getNumericCellValue())){
                                throw new BadRequestExceptions(Constants.ErrorOriginWarehouseStock);
                            }
                        }
                        ii++;
                    }
                    requestStockTransferItemList.add(requestStockTransferItem);
                    i++;
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
                            System.out.println(cell.getRichStringCellValue().getString().toUpperCase());
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
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
                    if(j>=1){
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
                    j++;
                }
                iStockTransaction.save("STO"+newStockTransfer.getId(),newStockTransfer.getOriginWarehouse(),requestStockTransactionItemList,"TRANSFERENCIA-SALIDA",user);
                iStockTransaction.save("STI"+newStockTransfer.getId(),newStockTransfer.getDestinationWarehouse(),requestStockTransactionItemList,"TRANSFERENCIA-ENTRADA",user);
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
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
    public CompletableFuture<ResponseSuccess> stockReturn(RequestStockReturnExcel requestStockReturnExcel, MultipartFile multipartFile) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Purchase purchase;
            StockReturn stockReturn;
            Warehouse warehouse;
            Shipment shipment;
            List<RequestStockReturnItem> requestStockReturnItemList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestStockReturnExcel.getTokenUser().toUpperCase());
                purchase = purchaseRepository.findBySerial(requestStockReturnExcel.getPurchaseSerial().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(), requestStockReturnExcel.getWarehouse().toUpperCase());
            }

            if(purchase == null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }else{
                stockReturn = stockReturnRepository.findBySerial(requestStockReturnExcel.getSerial());
                shipment = shipmentRepository.findByPurchaseIdAndShipmentTypeName(purchase.getId(), "EMBARQUE");
            }

            if(stockReturn != null){
                throw new BadRequestExceptions(Constants.ErrorStockReturnExists);
            }

            if(shipment == null){
                throw new BadRequestExceptions(Constants.ErrorShipment);
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
                        }
                        requestStockReturnItemList.add(requestStockReturnItem);
                        ii++;
                    }
                    i++;
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
                        if(j>=1&&(cell.getCellType()==NUMERIC)&&(ji==1)){
                            stockReturnItem.setQuantity((int) cell.getNumericCellValue());
                            requestStockTransactionItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        if(j>=1&&(cell.getCellType()==STRING)&&(ji==2)){
                            stockReturnItem.setObservations(cell.getRichStringCellValue().getString().toUpperCase());
                        }
                        ji++;
                    }
                    if(j>=1){
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
                    j++;
                }

                iStockTransaction.save("SR"+newStockReturn.getId(),shipment.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-PROVEEDOR",user);

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
                        if(i>=1 && (cell.getCellType()==NUMERIC)&&(ii==1)&&(orderItem != null)){
                            if(((int)cell.getNumericCellValue())<1){
                                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemZero);
                            }
                            if(((int)cell.getNumericCellValue())>orderItem.getQuantity()){
                                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentQuantity);
                            }
                            requestStockReplenishmentItem.setQuantity((int)cell.getNumericCellValue());
                        }
                        requestStockReplenishmentItemList.add(requestStockReplenishmentItem);
                        ii++;
                    }
                    i++;
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
                        if(j>=1 && (cell.getCellType() == NUMERIC) && (ji==1)){
                            stockReplenishmentItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        ji++;
                    }
                    if(j>=1){
                        stockReplenishmentItem.setClient(user.getClient());
                        stockReplenishmentItem.setClientId(user.getClientId());
                        stockReplenishmentItem.setRegistrationDate(new Date(System.currentTimeMillis()));
                        stockReplenishmentItem.setStatus(true);
                        stockReplenishmentItem.setTokenUser(user.getUsername());
                        stockReplenishmentItem.setStockReplenishment(newStockReplenishment);
                        stockReplenishmentItem.setStockReplenishmentId(newStockReplenishment.getId());
                        stockReplenishmentItemRepository.save(stockReplenishmentItem);
                    }
                    j++;
                }
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (IOException e) {
                throw new RuntimeException(e);
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
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase().substring(1));
                            if(product == null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                            if(orderItem == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
                            }
                            requestOrderStockItem.setProductSku(product.getSku());
                        }
                        if(i>=1 && (cell.getCellType() == STRING) && ii==1){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            requestOrderStockItem.setSupplierProductSerial(supplierProduct.getSerial());
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
                    if(i>=1){
                        requestOrderStockItemList.add(requestOrderStockItem);
                        Boolean existsStock = iOrderStockItem.checkWarehouseItemStock(ordering.getId(),warehouse,requestOrderStockItem).get();
                        if(!existsStock){
                            throw new BadRequestExceptions(Constants.ErrorOrderStockQuantity);
                        }
                    }
                    i++;
                }
                Set<String> serials = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                    if(!serials.add(requestOrderStockItem.getSupplierProductSerial())){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockDuplicateItem);
                    }
                }
                System.out.println(requestOrderStockItemList);
                Map<String,Integer> checkCount = requestOrderStockItemList.stream().collect(
                        Collectors.groupingBy(
                                RequestOrderStockItem::getProductSku,
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
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase().substring(1));
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
                    if(j>=1){
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
                    j++;
                }
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
                orderStock = orderStockRepository.findByOrderId(orderId);
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
                    if(i>=1){
                        requestOrderReturnItemList.add(requestOrderReturnItem);
                    }
                    i++;
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
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
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
                    if(j>=1){
                        orderReturnItem.setStatus(true);
                        orderReturnItem.setRegistrationDate(new Date(System.currentTimeMillis()));
                        orderReturnItem.setClient(user.getClient());
                        orderReturnItem.setClientId(user.getClientId());
                        orderReturnItem.setOrderId(orderStock.getOrderId());
                        orderReturnItem.setOrderReturn(newOrderReturn);
                        orderReturnItem.setOrderReturnId(newOrderReturn.getId());
                        orderReturnItem.setTokenUser(user.getUsername());
                        orderReturnItemRepository.save(orderReturnItem);
                        iGeneralStock.in(orderReturnItem.getSupplierProduct().getSerial(),orderReturnItem.getQuantity(),user.getUsername());
                        iWarehouseStock.in(orderStock.getWarehouse(),orderReturnItem.getSupplierProduct(),orderReturnItem.getQuantity(),user);
                        requestStockTransactionItemList.add(requestStockTransactionItem);
                    }
                    j++;
                }
                iStockTransaction.save("OR"+orderStock.getOrdering().getId(),orderStock.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-COMPRADOR",user);
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
