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
import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import static org.apache.poi.ss.usermodel.CellType.NUMERIC;
import static org.apache.poi.ss.usermodel.CellType.STRING;

@Service
@RequiredArgsConstructor
@Log4j2
public class ExcelImpl implements IExcel {
    private final UserRepository userRepository;
    private final SupplyOrderRepository supplyOrderRepository;
    private final WarehouseRepository warehouseRepository;
    private final SupplyOrderItemRepository supplyOrderItemRepository;
    private final IStockTransaction iStockTransaction;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final WarehouseStockRepository warehouseStockRepository;
    private final OrderingRepository orderingRepository;
    private final ProductRepository productRepository;
    private final OrderItemRepository orderItemRepository;
    private final ModelRepository modelRepository;
    private final ColorRepository colorRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final SizeRepository sizeRepository;
    private final UnitRepository unitRepository;
    private final IAudit iAudit;
    private final ProductPriceRepository productPriceRepository;
    private final UnitTypeRepository unitTypeRepository;
    private final BrandRepository brandRepository;
    private final IUtil iUtil;
    private final SubCategoryProductRepository subCategoryProductRepository;
    @Override
    public CompletableFuture<ResponseSuccess> purchase(RequestSupplyOrderExcel requestSupplyOrderExcel, MultipartFile multipartFile) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            SupplyOrder supplyOrder;
            try {
                user = userRepository.findByUsernameAndStatusTrue(requestSupplyOrderExcel.getTokenUser().toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestSupplyOrderExcel.getWarehouse().toUpperCase());
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


            try {

                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                Row supplierRow = sheet.getRow(0);
                Cell supplierCell = supplierRow.getCell(3);
                int i = 0;
                List<RequestStockTransactionItem> stockTransactionItemList = new ArrayList<>();
                List<RequestSupplyOrderItem> requestSupplyOrderItemList = new ArrayList<>();
                for(Row row:sheet){
                    RequestSupplyOrderItem requestSupplyOrderItem = RequestSupplyOrderItem.builder().build();

                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=2 && (cell.getCellType() == STRING) && (ii == 4)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
//                            if(supplierProduct == null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
//                            }
                            //requestSupplyOrderItem.setSupplierProduct(supplierProduct.getSerial());
                        }
                        if(i>=2 && (cell.getCellType() == NUMERIC) && (ii == 4)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
//                            if(supplierProduct == null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
//                            }
//                            requestSupplyOrderItem.setSupplierProduct(String.valueOf((int)(cell.getNumericCellValue())));
                        }
                        if(i>=2 && (cell.getCellType() == NUMERIC) && (ii==5)){
                            if(((int) cell.getNumericCellValue()) > 0){
                                requestSupplyOrderItem.setQuantity((int) cell.getNumericCellValue());
                            }
                        }
                        if(i>=2 && (cell.getCellType()==STRING) && (ii==6)){
                            requestSupplyOrderItem.setObservations(cell.getRichStringCellValue().getString().toUpperCase());
                        }
                        if(requestSupplyOrderItem.getObservations()==null){
                            requestSupplyOrderItem.setObservations("NO APLICA");
                        }
                        ii++;
                    }
                    if(i>=2){
//                        System.out.println(requestSupplyOrderItem.getQuantity());
//                        System.out.println(requestSupplyOrderItem.getSupplierProduct());
                    }
//                    if(i>=2 && (
//                            requestSupplyOrderItem.getQuantity() != null &&
//                            requestSupplyOrderItem.getQuantity() > 0 &&
//                                    requestSupplyOrderItem.getSupplierProduct() != null)){
//                        requestSupplyOrderItemList.add(requestSupplyOrderItem);
//                    }
//                    if(i>=2 && (
//                            requestSupplyOrderItem.getQuantity() == null ||
//                            requestSupplyOrderItem.getQuantity() < 1 ||
//                                    requestSupplyOrderItem.getSupplierProduct() == null)){
//                        continue;
//                    }
                    i++;
                }
                if(requestSupplyOrderItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorPurchaseItemZero);
                }
                Set<String> serials = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestSupplyOrderItem requestSupplyOrderItem : requestSupplyOrderItemList){
//                    if(!serials.add(requestSupplyOrderItem.getSupplierProduct())){
//                        hasDuplicate = true;
//                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorPurchaseDuplicateItem);
                    }
                }
//                SupplyOrder newSupplyOrder = supplyOrderRepository.save(SupplyOrder.builder()
//                        .ref(requestSupplyOrderExcel.getSerial().toUpperCase())
//                        .status(true)
//                        .registrationDate(OffsetDateTime.now())
//                        .updateDate(OffsetDateTime.now())
//                        .supplyOrderDocument(supplyOrderDocument)
//                        .purchaseDocumentId(supplyOrderDocument.getId())
//                        .warehouse(warehouse)
//                        .warehouseId(warehouse.getId())
//                        .supplyOrderType(supplyOrderType)
//                        .purchaseTypeId(supplyOrderType.getId())
//                        .client(user.getClient())
//                        .clientId(user.getClientId())
//                        .user(user)
//                        .userId(user.getId())
//                        .build());
                int j = 0;
                for(Row row: sheet){
                    SupplyOrderItem supplyOrderItem = SupplyOrderItem.builder().build();
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    int ji = 0;
                    for(Cell cell:row){
                        if(j>=2 && (cell.getCellType() == STRING) && (ji == 4)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            //requestStockTransactionItem.setSupplierProductSerial(cell.getRichStringCellValue().getString().toUpperCase());
//                            supplyOrderItem.setSupplierProduct(supplierProduct);
//                            supplyOrderItem.setSupplierProductId(supplierProduct.getId());
                        }
                        if(j>=2 && (cell.getCellType() == NUMERIC) && (ji == 4)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int) (cell.getNumericCellValue())));
                            //requestStockTransactionItem.setSupplierProductSerial(cell.getRichStringCellValue().getString().toUpperCase());
//                            supplyOrderItem.setSupplierProduct(supplierProduct);
//                            supplyOrderItem.setSupplierProductId(supplierProduct.getId());
                        }
                        if(j>=2 && (cell.getCellType()==NUMERIC)&&(ji == 5)){
                            if(cell.getNumericCellValue() > 0){
                                supplyOrderItem.setQuantity((int) cell.getNumericCellValue());
                                requestStockTransactionItem.setQuantity((int) cell.getNumericCellValue());
                            }

                        }
                        if(j>=2 && (cell.getCellType() == STRING) && (ji == 6)){
                            supplyOrderItem.setObservations(cell.getRichStringCellValue().getString().toUpperCase());
                        }

                        ji++;
                    }
                    j++;
                }
//                iStockTransaction.save("S"+ requestSupplyOrderExcel.getSerial().toUpperCase(), warehouse,stockTransactionItemList,"COMPRA",user);
//                iAudit.save("ADD_PURCHASE_EXCEL","COMPRA "+ newSupplyOrder.getRef()+" CREADA POR EXCEL.", newSupplyOrder.getRef(),user.getUsername());
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
            List<RequestStockTransferItem> requestStockTransferItemList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestStockTransferExcel.getTokenUser().toUpperCase());
                originWarehouse = warehouseRepository.findByNameAndStatusTrue(requestStockTransferExcel.getOriginWarehouse().toUpperCase());
                destinationWarehouse = warehouseRepository.findByNameAndStatusTrue(requestStockTransferExcel.getDestinationWarehouse().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
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
                    RequestStockTransferItem requestStockTransferItem = RequestStockTransferItem.builder().build();
                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && (ii==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
//                            if(supplierProduct == null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
//                            }
                            //requestStockTransferItem.setSupplierProductSerial(supplierProduct.getSerial());
//                            originWarehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(originWarehouse.getId(),supplierProduct.getId());
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
//                            if(supplierProduct == null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
//                            }
                            //requestStockTransferItem.setSupplierProductSerial(supplierProduct.getSerial());
//                            originWarehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(originWarehouse.getId(),supplierProduct.getId());
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
//                    if(i>=1 && (
//                            requestStockTransferItem.getQuantity() != null &&
//                                    requestStockTransferItem.getSupplierProductSerial() != null)){
//                        requestStockTransferItemList.add(requestStockTransferItem);
//                    }
//                    if(i>=1 && (
//                            requestStockTransferItem.getQuantity() == null ||
//                                    requestStockTransferItem.getSupplierProductSerial() == null)){
//                        break;
//                    }
                    i++;
                }
                if(requestStockTransferItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorStockTransferItemZero);
                }
                Set<String> skus = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestStockTransferItem requestStockTransferItem : requestStockTransferItemList){
//                    if(!skus.add(requestStockTransferItem.getSupplierProductSerial())){
//                        hasDuplicate = true;
//                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorStockTransferDuplicateItem);
                    }
                }

                List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();
                int j = 0;
                for(Row row:sheet){
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    int ji = 0;
                    for(Cell cell:row){
                        if(j>=1 && (cell.getCellType() == STRING) && (ji==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
//                            stockTransferItem.setSupplierProduct(supplierProduct);
//                            stockTransferItem.setSupplierProductId(supplierProduct.getId());
                            //requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && (ji==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int) (cell.getNumericCellValue())));
//                            stockTransferItem.setSupplierProduct(supplierProduct);
//                            stockTransferItem.setSupplierProductId(supplierProduct.getId());
                            //requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        ji++;
                    }
                    j++;
                }
//                iStockTransaction.save("STO"+newStockTransfer.getId(),newStockTransfer.getOriginWarehouse(),requestStockTransactionItemList,"TRANSFERENCIA-SALIDA",user);
//                iStockTransaction.save("STI"+newStockTransfer.getId(),newStockTransfer.getDestinationWarehouse(),requestStockTransactionItemList,"TRANSFERENCIA-ENTRADA",user);
                //iAudit.save("ADD_STOCK_TRANSFER_EXCEL","TRANSFERENCIA DE STOCK "+newStockTransfer.getSerial()+" CREADA POR EXCEL.",newStockTransfer.getSerial(),user.getUsername());
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
            Warehouse warehouse;
            List<RequestStockReturnItem> requestStockReturnItemList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestStockReturnExcel.getTokenUser().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            try {
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                Row headerDataRow = sheet.getRow(0);
                Cell supplierCell = headerDataRow.getCell(3);
                Cell warehouseCell = headerDataRow.getCell(5);
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(), warehouseCell.getRichStringCellValue().getString().toUpperCase());
                if(warehouse==null){
                    throw new BadRequestExceptions(Constants.ErrorWarehouse);
                }
                int i = 0;
                for(Row row:sheet){
                    WarehouseStock warehouseStock;
                    RequestStockReturnItem requestStockReturnItem = RequestStockReturnItem.builder().build();
                    int ii = 0;
                    for(Cell cell:row){
                        if(i>=2 && (cell.getCellType() == STRING) && (ii==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            //requestStockReturnItem.setSupplierProduct(supplierProduct.getSerial());
                        }
                        if(i>=2 && (cell.getCellType() == NUMERIC) && (ii==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            //requestStockReturnItem.setSupplierProduct(supplierProduct.getSerial());
                        }
                        if(i>=2&&(cell.getCellType() == STRING) && (ii==8)){
                            requestStockReturnItem.setObservations(cell.getRichStringCellValue().getString().toUpperCase());
                        }
                        if(requestStockReturnItem.getObservations()==null){
                            requestStockReturnItem.setObservations("NO APLICA");
                        }
                        ii++;
                    }
                    System.out.println(requestStockReturnItem);
//                    if(i>=2 && (
//                            requestStockReturnItem.getQuantity() != null &&
//                                    requestStockReturnItem.getQuantity() > 0 &&
//                                    requestStockReturnItem.getObservations() != null &&
//                                    requestStockReturnItem.getSupplierProduct() != null)
//                    ){
//                        requestStockReturnItemList.add(requestStockReturnItem);
//                    }
//                    if(i>=2 && (
//                            requestStockReturnItem.getQuantity() == null ||
//                                    requestStockReturnItem.getQuantity() < 1 ||
//                                    requestStockReturnItem.getObservations() == null ||
//                                    requestStockReturnItem.getSupplierProduct() == null)){
//                        continue;
//                    }
                    i++;
                }
                if(requestStockReturnItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnItemZero);
                }
                Set<String> skus = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestStockReturnItem requestStockReturnItem : requestStockReturnItemList){
//                    if(!skus.add(requestStockReturnItem.getSupplierProduct())){
//                        hasDuplicate = true;
//                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorStockReturnDuplicateItem);
                    }
                }
                List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();

                int j = 0;
                for(Row row:sheet){
                    int ji = 0;
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    for(Cell cell:row){
                        if(j>=2 && (cell.getCellType() == STRING) && (ji==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            //requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
//                            stockReturnItem.setSupplierProduct(supplierProduct);
//                            stockReturnItem.setSupplierProductId(supplierProduct.getId());
                        }
                        if(j>=2 && (cell.getCellType() == NUMERIC) && (ji==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
                            //requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
//                            stockReturnItem.setSupplierProduct(supplierProduct);
//                            stockReturnItem.setSupplierProductId(supplierProduct.getId());
                        }
                        ji++;
                    }
                    j++;
                }

//                iStockTransaction.save("SR"+newStockReturn.getId(), newStockReturn.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-PROVEEDOR",user);
//                iAudit.save("ADD_STOCK_RETURN_EXCEL","DEVOLUCION DE STOCK "+newStockReturn.getSerial()+" CREADA POR EXCEL.",newStockReturn.getSerial(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> stockReplenishment(UUID orderId, MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            List<RequestStockReplenishmentItem> requestStockReplenishmentItemList = new ArrayList<>();
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
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
//                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
//                            if(product == null){
//                                throw new BadRequestExceptions(Constants.ErrorProduct);
//                            }
//                            requestStockReplenishmentItem.setProductSku(iUtil.buildProductSku(product));
//                            orderItem = orderItemRepository.findByOrderIdAndProductId(orderId,product.getId());
//                            if(orderItem == null){
//                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
//                            }
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==0)){
//                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int) (cell.getNumericCellValue())));
//                            if(product == null){
//                                throw new BadRequestExceptions(Constants.ErrorProduct);
//                            }
//                            requestStockReplenishmentItem.setProductSku(iUtil.buildProductSku(product));
//                            orderItem = orderItemRepository.findByOrderIdAndProductId(orderId,product.getId());
//                            if(orderItem == null){
//                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
//                            }
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==0)){
//                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
//                            if(product == null){
//                                throw new BadRequestExceptions(Constants.ErrorProduct);
//                            }
//                            requestStockReplenishmentItem.setProductSku(iUtil.buildProductSku(product));
//                            orderItem = orderItemRepository.findByOrderIdAndProductId(orderId,product.getId());
//                            if(orderItem == null){
//                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
//                            }
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
//                    if(i>=1 && (
//                            requestStockReplenishmentItem.getQuantity() != null &&
//                                    requestStockReplenishmentItem.getProductSku() != null)){
//                        requestStockReplenishmentItemList.add(requestStockReplenishmentItem);
//                    }
//                    if(i>=1 && (
//                            requestStockReplenishmentItem.getQuantity() == null ||
//                                    requestStockReplenishmentItem.getProductSku() == null)){
//                        break;
//                    }
                    i++;
                }
                if(requestStockReplenishmentItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemZero);
                }
                Set<String> skus = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestStockReplenishmentItem requestStockReplenishmentItem : requestStockReplenishmentItemList){
//                    if(!skus.add(requestStockReplenishmentItem.getProductSku())){
//                        hasDuplicate = true;
//                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorStockReplenishmentDuplicateItem);
                    }
                }
                int j = 0;
                for(Row row:sheet){
                    int ji = 0;
                    Product product;
                    for(Cell cell:row){
                        if(j>=1 && (cell.getCellType()==STRING)&&(ji==0)){
                            //product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
                            //stockReplenishmentItem.setProduct(product);
                            //stockReplenishmentItem.setProductId(product.getId());=
                        }
                        if(j>=1 && (cell.getCellType()==NUMERIC)&&(ji==0)){
//                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
//                            stockReplenishmentItem.setProduct(product);
//                            stockReplenishmentItem.setProductId(product.getId());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && (ji==1)){
                        }
                        ji++;
                    }
                    j++;
                }
                //iAudit.save("ADD_STOCK_REPLENISHMENT_EXCEL","RESTOCK "+newStockReplenishment.getOrderId()+" CREADO POR EXCEL.",newStockReplenishment.getOrderId().toString(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> orderStock(UUID orderId,String warehouseName, MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions {
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
                    RequestOrderStockItem requestOrderStockItem = RequestOrderStockItem.builder().build();
                    for(Cell cell:row){
                        if(i>=1 && (cell.getCellType() == STRING) && ii==0){
//                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
//                            if(product == null){
//                                throw new BadRequestExceptions(Constants.ErrorProduct);
//                            }
//                            orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
//                            if(orderItem == null){
//                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
//                            }
//                            requestOrderStockItem.setProduct(iUtil.buildProductSku(product));
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && ii==0){
//                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
//                            if(product == null){
//                                throw new BadRequestExceptions(Constants.ErrorProduct);
//                            }
//                            orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
//                            if(orderItem == null){
//                                throw new BadRequestExceptions(Constants.ErrorOrderItem);
//                            }
//                            requestOrderStockItem.setProduct(iUtil.buildProductSku(product));
                        }
                        if(i>=1 && (cell.getCellType() == STRING) && ii==1){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
//                            if(supplierProduct == null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
//                            }
                            //requestOrderStockItem.setSupplierProduct(supplierProduct.getSerial());
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && ii==1){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
//                            if(supplierProduct == null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
//                            }
                            //requestOrderStockItem.setSupplierProduct(supplierProduct.getSerial());
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
//                    if(i>=1 && (
//                            requestOrderStockItem.getQuantity() != null &&
//                                    requestOrderStockItem.getProduct() != null &&
//                                    requestOrderStockItem.getSupplierProduct() != null)){
//                        requestOrderStockItemList.add(requestOrderStockItem);
//                    }
//                    if(i>=1 && (
//                            requestOrderStockItem.getQuantity() == null ||
//                                    requestOrderStockItem.getProduct() == null ||
//                                    requestOrderStockItem.getSupplierProduct() == null)){
//                        break;
//                    }
                    i++;
                }
                if(requestOrderStockItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorOrderStockItemZero);
                }
                Set<String> serials = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
//                    if(!serials.add(requestOrderStockItem.getSupplierProduct())){
//                        hasDuplicate = true;
//                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockDuplicateItem);
                    }
                }
//                Map<String,Integer> checkCount = requestOrderStockItemList.stream().collect(
//                        Collectors.groupingBy(
//                                RequestOrderStockItem::getProduct,
//                                Collectors.summingInt(RequestOrderStockItem::getQuantity)
//                        )
//                );
//                checkCount.forEach((key,value)->{
//                    Product product = productRepository.findBySkuAndStatusTrue(key);
//                    OrderItem orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
//                    if(value > orderItem.getQuantity()){
//                        throw new BadRequestExceptions(Constants.ErrorOrderStockProductQuantity);
//                    }
//                });
                int j = 0;
                for(Row row :sheet){
                    int ji = 0;
                    Product product;
                    OrderItem orderItem;
                    for(Cell cell:row){
                        if(j>=1 && (cell.getCellType() == STRING) && ji==0){
//                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
//                            orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
//                            orderStockItem.setOrderItem(orderItem);
//                            orderStockItem.setOrderItemId(orderItem.getId());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && ji==0){
//                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
//                            orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
//                            orderStockItem.setOrderItem(orderItem);
//                            orderStockItem.setOrderItemId(orderItem.getId());
                        }
                        if(j>=1 && (cell.getCellType() == STRING) && ji==1){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getStringCellValue().toUpperCase());
//                            orderStockItem.setSupplierProduct(supplierProduct);
//                            orderStockItem.setSupplierProductId(supplierProduct.getId());
                        }
                        ji++;
                    }
                    j++;
                }
                //iAudit.save("ADD_ORDER_STOCK_EXCEL","PREPARACION DEL PEDIDO #"+orderStock.getOrderId()+" CREADO POR EXCEL.",orderStock.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            } catch (IOException | RuntimeException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> orderReturn(UUID orderId, MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try {
                InputStream inputStream = multipartFile.getInputStream();
                Workbook workbook = WorkbookFactory.create(inputStream);
                Sheet sheet = workbook.getSheetAt(0);
                List<RequestOrderReturnItem> requestOrderReturnItemList = new ArrayList<>();
                int i = 0;
                for(Row row:sheet){
                    Product product;
                    RequestOrderReturnItem requestOrderReturnItem = RequestOrderReturnItem.builder().build();
                    int ii = 0;
                    for(Cell cell:row){
                        if((i>=1) && (cell.getCellType() == STRING) && (ii==0)){
//                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
//                            if(product==null){
//                                throw new BadRequestExceptions(Constants.ErrorProduct);
//                            }
//                            requestOrderReturnItem.setProductSku(iUtil.buildProductSku(product));
                        }
                        if((i>=1) && (cell.getCellType() == NUMERIC) && (ii==0)){
//                            product = productRepository.findBySkuAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
//                            if(product==null){
//                                throw new BadRequestExceptions(Constants.ErrorProduct);
//                            }
//                            requestOrderReturnItem.setProductSku(iUtil.buildProductSku(product));
                        }
                        if(i>=1 && (cell.getCellType() == STRING) && (ii==1)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getRichStringCellValue().getString().toUpperCase());
//                            if(supplierProduct==null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
//                            }
//                            orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
                            //requestOrderReturnItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        if(i>=1 && (cell.getCellType() == NUMERIC) && (ii==1)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int) (cell.getNumericCellValue())));
//                            if(supplierProduct==null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
//                            }
//                            orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
                            //requestOrderReturnItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        ii++;
                    }
//                    if(i>=1 && (
//                            requestOrderReturnItem.getQuantity()!=null &&
//                                    requestOrderReturnItem.getOrderReturnType() !=null &&
//                                    requestOrderReturnItem.getProductSku() != null &&
//                                    requestOrderReturnItem.getSupplierProductSerial() != null)){
//                        requestOrderReturnItemList.add(requestOrderReturnItem);
//                    }
//                    if(i>=1 && (
//                            requestOrderReturnItem.getQuantity()==null ||
//                                    requestOrderReturnItem.getOrderReturnType() == null ||
//                                    requestOrderReturnItem.getProductSku() == null ||
//                                    requestOrderReturnItem.getSupplierProductSerial() == null)){
//                        break;
//                    }
                    i++;
                }
                if(requestOrderReturnItemList.isEmpty()){
                    throw new BadRequestExceptions(Constants.ErrorOrderReturnItemZero);
                }
                Set<String> serials = new HashSet<>();
                boolean hasDuplicate = false;
                for(RequestOrderReturnItem requestOrderReturnItem : requestOrderReturnItemList){
//                    if(!serials.add(requestOrderReturnItem.getSupplierProductSerial())){
//                        hasDuplicate = true;
//                    }
                    if(hasDuplicate){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockDuplicateItem);
                    }
                }
//                Map<String,Integer> checkCount = requestOrderReturnItemList.stream().collect(
//                        Collectors.groupingBy(
//                                    RequestOrderReturnItem::getSupplierProductSerial,
//                                    Collectors.summingInt(RequestOrderReturnItem::getQuantity)
//                        )
//                );
//                checkCount.forEach((key,value)->{
//                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(key);
//                    OrderStockItem orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
//                    if(value > orderStockItem.getQuantity()){
//                        throw new BadRequestExceptions(Constants.ErrorOrderStockProductQuantity);
//                    }
//                });
                List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();
                int j = 0;
                for(Row row:sheet){
                    int ji = 0;
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    for(Cell cell:row){
                        Product product;
                        if(j>=1 && (cell.getCellType() == STRING) && (ji==0)){
//                            product = productRepository.findBySkuAndStatusTrue(cell.getRichStringCellValue().getString().substring(1).toUpperCase());
//                            orderReturnItem.setProduct(product);
//                            orderReturnItem.setProductId(product.getId());
                        }
                        if(j>=1 && (cell.getCellType() == STRING) && (ji==1)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getStringCellValue().toUpperCase());
//                            orderReturnItem.setSupplierProduct(supplierProduct);
//                            orderReturnItem.setSupplierProductId(supplierProduct.getId());
                            //requestStockTransactionItem.setSupplierProductSerial(supplierProduct.getSerial());
                        }
                        if(j>=1 && (cell.getCellType() == NUMERIC) && (ji==2)){
                            requestStockTransactionItem.setQuantity((int) cell.getNumericCellValue());
                        }
                        if(j>=1 && (cell.getCellType() == STRING) && (ji==3)){
                        }
                        ji++;
                    }
                    j++;
                }
                //iStockTransaction.save("OR"+orderStock.getOrdering().getId(),orderStock.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-COMPRADOR",user);
                //iAudit.save("ADD_ORDER_RETURN_EXCEL","DEVOLUCION DE PEDIDO "+newOrderReturn.getOrderId()+" CREADA POR EXCEL.",newOrderReturn.getOrderId().toString(),user.getUsername());
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
                Set<String> names = new HashSet<>();
                Set<String> uniqueCombinations = new HashSet<>();
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
                    SubCategoryProduct subCategoryProduct=null;
                    Product newProduct = Product.builder().build();
                    ProductPrice productPrice = ProductPrice.builder().build();
                    for(int ii = 0;ii <= 8;ii++){
                        if((i>=1)&&(
                                (row.getCell(0)==null) ||
                                        (row.getCell(1)==null)) ||
                                (row.getCell(2)==null) ||
                                (row.getCell(3)==null) ||
                                (row.getCell(4)==null) ||
                                (row.getCell(5)==null) ||
                                (row.getCell(6)==null) ||
                                (row.getCell(7)==null) ||
                                (row.getCell(8)==null)
                        ){
                            break;
                        }
                        if((i>=1) && (row.getCell(0).getCellType() == STRING) && (ii==0)){
                            product = productRepository.findByNameAndClientId(row.getCell(0).getRichStringCellValue().getString().toUpperCase(),user.getClientId());
                            if(product!=null){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorProductExists)
                                        .build();
                            }
                            newProduct.setName(row.getCell(0).getStringCellValue().toUpperCase());
                        }
                        if((i>=1) && (row.getCell(0).getCellType() == NUMERIC) && (ii==0)){
                            product = productRepository.findByNameAndClientId(String.valueOf((int) (row.getCell(0).getNumericCellValue())),user.getClientId());
                            if(product!=null){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorProductExists)
                                        .build();
                            }
                            newProduct.setName(String.valueOf((int) (row.getCell(0).getNumericCellValue())));
                        }
                        if((i>=1)&&(row.getCell(2).getCellType() == STRING) && (ii==2)){
                            model = modelRepository.findByNameAndClientIdAndStatusTrue(row.getCell(2).getRichStringCellValue().getString().toUpperCase(),user.getClientId());
                            if(model == null){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorModel)
                                        .build();
                            }
                            newProduct.setModel(model);
                            newProduct.setModelId(model.getId());
                        }
                        if((i>=1)&&(row.getCell(3).getCellType()==STRING)&&(ii==3)){
                            color = colorRepository.findByNameAndClientIdAndStatusTrue(row.getCell(3).getRichStringCellValue().getString().toUpperCase(),user.getClientId());
                            if(color==null){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorColor)
                                        .build();
                            }
                            newProduct.setColor(color);
                            newProduct.setColorId(color.getId());
                        }
                        if((i>=1)&&(row.getCell(5).getCellType()==STRING)&&(ii==5)){
                            subCategoryProduct = subCategoryProductRepository.findByNameAndClientIdAndStatusTrue(
                                    row.getCell(5).getRichStringCellValue().getString().toUpperCase(),
                                    user.getClientId()
                            );
                            if(subCategoryProduct==null){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorSubCategoryProduct)
                                        .build();
                            }
                            newProduct.setSubCategoryProduct(subCategoryProduct);
                            newProduct.setSubCategoryProductId(subCategoryProduct.getId());
                        }
                        if((i>=1)&&(row.getCell(6).getCellType()==STRING)&&(ii==6)&&(subCategoryProduct!=null)){
                            size = sizeRepository.findByNameAndStatusTrueAndClientId(row.getCell(6).getRichStringCellValue().getString().toUpperCase(),user.getClientId());
                            if(size==null){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorSize)
                                        .build();
                            }

                            if(!Objects.equals(size.getSizeTypeId(), subCategoryProduct.getCategoryProduct().getSizeTypeId())){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorSizeTypeCategoryProduct)
                                        .build();
                            }
                            newProduct.setSize(size);
                            newProduct.setSizeId(size.getId());
                        }
                        if((i>=1)&&(row.getCell(6).getCellType()==NUMERIC)&&(ii==6)&&(subCategoryProduct!=null)){
                            size = sizeRepository.findByNameAndStatusTrueAndClientId(String.valueOf((int) row.getCell(6).getNumericCellValue()),user.getClientId());
                            if(size==null){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorSize)
                                        .build();
                            }
                            if(!Objects.equals(size.getSizeTypeId(), subCategoryProduct.getCategoryProduct().getSizeTypeId())){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorSizeTypeCategoryProduct)
                                        .build();
                            }
                            newProduct.setSize(size);
                            newProduct.setSizeId(size.getId());
                        }
                        if((i>=1)&&(row.getCell(7).getCellType()==STRING)&&(ii==7)&&(subCategoryProduct!=null)){
                            unit = unitRepository.findByNameAndUnitTypeIdAndClientIdAndStatusTrue(row.getCell(7).getRichStringCellValue().getString().toUpperCase(),subCategoryProduct.getCategoryProduct().getUnitTypeId(),user.getClientId());
                            if(unit==null){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorUnit)
                                        .build();
                            }
                            newProduct.setUnit(unit);
                            newProduct.setUnitId(unit.getId());
                        }
                        if((i>=1)&&(row.getCell(8).getCellType()==NUMERIC)&&(ii==8)){
                            if(row.getCell(8).getNumericCellValue() < 0.01){
                                return ResponseSuccess.builder()
                                        .code(400)
                                        .message(Constants.ErrorProductPriceZero)
                                        .build();
                            }
                            productPrice.setUnitSalePrice(row.getCell(8).getNumericCellValue());
                        }
                    }
                    if(i>=1 && (
                            newProduct.getName() != null &&
                                    newProduct.getSize() != null &&
                                    newProduct.getUnit() != null &&
                                    newProduct.getSubCategoryProduct() != null &&
                                    newProduct.getModel() != null &&
                                    productPrice.getUnitSalePrice() != null &&
                                    newProduct.getColor() != null
                            )){
                        newProduct.setStatus(true);
                        newProduct.setPictureFlag(false);
                        newProduct.setRegistrationDate(OffsetDateTime.now());
                        newProduct.setUpdateDate(OffsetDateTime.now());
                        newProduct.setUser(user);
                        newProduct.setUserId(user.getId());
                        newProduct.setClient(user.getClient());
                        newProduct.setClientId(user.getClientId());
                        productPrice.setUser(user);
                        newProduct.setUserId(user.getId());
                        productPrice.setRegistrationDate(OffsetDateTime.now());
                        productPrice.setUpdateDate(OffsetDateTime.now());
                        productPrice.setStatus(true);
                        products.add(newProduct);
                        productPrices.add(productPrice);
                        uniqueCombinations.add(iUtil.getUniqueProductKey(newProduct));
                    }
                    if(i>=1 && (
                            newProduct.getName() == null ||
                                    newProduct.getSize() == null ||
                                    newProduct.getUnit() == null ||
                                    productPrice.getUnitSalePrice() == null ||
                                    newProduct.getModel() == null ||
                                    newProduct.getSubCategoryProduct() == null ||
                                    newProduct.getColor() == null
                    )){
                        continue;
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
                    Product existingProduct = null;
                    Product product = products.get(j);
                    ProductPrice productPrice = productPrices.get(j);
                    if(!names.add(product.getName())||
                            !uniqueCombinations.add(iUtil.buildProductSku(product))){
                        hasDuplicate = true;
                    }
                    if(hasDuplicate){
                        return ResponseSuccess.builder()
                                .code(400)
                                .message(Constants.ErrorExcelDuplicatedRecordFile)
                                .build();
                    }else {
                        Product storedProduct = productRepository.save(product);
                        productPrice.setProductId(storedProduct.getId());
                        productPrice.setProduct(storedProduct);
                        productPriceRepository.save(productPrice);
                        iAudit.save("ADD_PRODUCT_EXCEL","PRODUCTO "+product.getName()+" CREADO POR EXCEL.",product.getName(),user.getUsername());
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
                Set<String> skus = new HashSet<>();
                boolean hasDuplicate = false;
                int i = 0;
                for(Row row:sheet){
                    int ii = 0;
                    Product product;
                    for(Cell cell:row){
                        if((i>=1) && (cell.getCellType() == STRING) && (ii==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(cell.getStringCellValue().toUpperCase());
//                            if(supplierProduct!=null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierExists);
//                            }
                            //newSupplierProduct.setSerial(cell.getStringCellValue().toUpperCase());
                        }
                        if((i>=1) && (cell.getCellType() == NUMERIC) && (ii==0)){
//                            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(String.valueOf((int)(cell.getNumericCellValue())));
//                            if(supplierProduct!=null){
//                                throw new BadRequestExceptions(Constants.ErrorSupplierExists);
//                            }
                            //newSupplierProduct.setSerial(String.valueOf((int)(cell.getNumericCellValue())));
                        }
                        if((i>=1) && (cell.getCellType() == STRING) && (ii==1)){
//                            product = productRepository.findBySkuAndStatusTrue(cell.getStringCellValue().toUpperCase());
//                            if(product == null){
//                                throw new BadRequestExceptions(Constants.ErrorProduct);
//                            }
//                            newSupplierProduct.setProduct(product);
//                            newSupplierProduct.setProductId(product.getId());
                        }
                        if((i>=1) && (cell.getCellType() == STRING) && (ii==2)){

                        }
                        if((i>=1) && (cell.getCellType() == NUMERIC) && (ii==3)){
                            if(cell.getNumericCellValue() < 0.01){
                                throw new BadRequestExceptions(Constants.ErrorSupplierProductZero);
                            }
                        }
                        ii++;
                    }
                    i++;
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
                        newModel.setRegistrationDate(OffsetDateTime.now());
                        newModel.setUpdateDate(OffsetDateTime.now());
                        newModel.setUser(user);
                        newModel.setUserId(user.getId());
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
