package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.*;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IExcel;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.services.IStockTransaction;
import com.proyect.masterdata.services.IWarehouseStock;
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
                Map<Integer, List<String>> data = new HashMap<>();
                int i = 0;
                for(Row row:sheet){
                    SupplierProduct supplierProduct;
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType()==STRING)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            System.out.println(cell);
                        }
                    }
                    i++;
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
                    data.put(j,new ArrayList<>());
                    for(Cell cell:row){
                        if(j>=1 && (cell.getCellType()==STRING)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            purchaseItem.setSupplierProduct(supplierProduct);
                            purchaseItem.setSupplierProductId(supplierProduct.getId());
                            System.out.println(cell);
                        }

                        if(j>=1 && (cell.getCellType()==NUMERIC)){
                            System.out.println("quantity");
                            purchaseItem.setQuantity((int) cell.getNumericCellValue());
                            if(purchaseItem.getQuantity() == null){
                                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                            }
                            System.out.println(cell);
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
                for(Row row:sheet){
                    SupplierProduct supplierProduct;
                    PurchaseItem purchaseItem;
                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && (ii == 0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                            if(purchaseItem == null){
                                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                            }
                        }
                        ii++;
                    }
                    i++;
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

                    WarehouseStock originWarehouseStock;
                    int ii = 0;
                    for(Cell cell:row){
                        SupplierProduct supplierProduct = null;
                        if(i>=1 && (cell.getCellType() == STRING) && (ii==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==1) && (supplierProduct != null)){
                            originWarehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(originWarehouse.getId(),supplierProduct.getId());
                            if(originWarehouseStock.getQuantity() < ((int) cell.getNumericCellValue())){
                                throw new BadRequestExceptions(Constants.ErrorOriginWarehouseStock);
                            }
                        }
                        ii++;
                    }
                    i++;
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
                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && (ii==0)){
                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(supplierProduct == null){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                            }
                            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                            if(purchaseItem == null){
                                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                            }
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==1) && (supplierProduct != null)){
                            warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),supplierProduct.getId());
                            if(((int) cell.getNumericCellValue()) > warehouseStock.getQuantity()){
                                throw new BadRequestExceptions(Constants.ErrorStockReturnWarehouseQuantity);
                            }
                            if(((int) cell.getNumericCellValue()) > purchaseItem.getQuantity()){
                                throw new BadRequestExceptions(Constants.ErrorStockReturnQuantity);
                            }
                        }
                        ii++;
                    }
                    i++;
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
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && (ii==0)){
                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            if(product == null){
                                throw new BadRequestExceptions(Constants.ErrorProduct);
                            }
                            orderItem = orderItemRepository.findByOrderIdAndProductId(orderId,product.getId());
                            if(orderItem == null){
                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
                            }
                        }
                        if(i>=1 && (cell.getCellType()==NUMERIC)&&(ii==1)&&(orderItem != null)){
                            if((cell.getNumericCellValue())>orderItem.getQuantity()){
                                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentQuantity);
                            }
                        }
                        ii++;
                    }
                    i++;
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
}
