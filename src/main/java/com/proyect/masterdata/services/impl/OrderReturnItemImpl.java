package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.services.IOrderReturnItem;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderReturnItemImpl implements IOrderReturnItem {
    private final UserRepository userRepository;
    private final OrderReturnItemRepository orderReturnItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final OrderReturnTypeRepository orderReturnTypeRepository;
    private final ProductRepository productRepository;
    private final OrderingRepository orderingRepository;
    private final OrderItemRepository orderItemRepository;
    private final OrderReturnRepository orderReturnRepository;
    private final OrderStockRepository orderStockRepository;
    private final OrderStockItemRepository orderStockItemRepository;
    private final IGeneralStock iGeneralStock;
    private final IWarehouseStock iWarehouseStock;
    private final GeneralStockRepository generalStockRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final IAudit iAudit;
    private final OrderReturnItemRepositoryCustom orderReturnItemRepositoryCustom;
    private final WarehouseRepository warehouseRepository;
    @Override
    public CompletableFuture<ResponseSuccess> save(Long orderId, RequestOrderReturnItem requestOrderReturnItem, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            OrderReturnType orderReturnType;
            OrderReturnItem orderReturnItem;
            OrderReturn orderReturn;
            OrderItem orderItem;
            Product product;
            OrderStock orderStock;
            OrderStockItem orderStockItem;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderReturnItem.getSupplierProductSerial().toUpperCase());
                orderReturnType = orderReturnTypeRepository.findByNameAndStatusTrue(requestOrderReturnItem.getOrderReturnType().toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(requestOrderReturnItem.getProductSku().toUpperCase());
                orderReturn = orderReturnRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderReturn == null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturn);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }
            if(orderReturnType==null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnType);
            }else{
                orderItem = orderItemRepository.findByOrderIdAndProductId(orderId, product.getId());
            }
            if(orderItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }else{
                orderStock = orderStockRepository.findByOrderId(orderItem.getOrderId());
            }
            if(orderStock == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }else{
                orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
            }
            if(orderStockItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItem);
            }else{
                orderReturnItem = orderReturnItemRepository.findByClientIdAndOrderReturnIdAndSupplierProductIdAndStatusTrue(user.getClientId(),orderReturn.getId(),supplierProduct.getId());
            }
            if(orderReturnItem != null){
                throw new BadRequestExceptions(Constants.ErrorOrderItemExists);
            }
            if(requestOrderReturnItem.getQuantity() > orderStockItem.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnItemQuantity);
            }
            try {
                OrderReturnItem newOrderReturnItem = orderReturnItemRepository.save(OrderReturnItem.builder()
                        .orderReturn(orderReturn)
                        .orderReturnId(orderReturn.getId())
                        .orderReturnType(orderReturnType)
                        .orderReturnTypeId(orderReturnType.getId())
                        .product(product)
                        .productId(product.getId())
                        .quantity(requestOrderReturnItem.getQuantity())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .status(true)
                        .build());
                iGeneralStock.in(supplierProduct.getSerial(), requestOrderReturnItem.getQuantity(), user.getUsername());
                iWarehouseStock.in(orderStock.getWarehouse(),supplierProduct, requestOrderReturnItem.getQuantity(), user);
                iAudit.save("ADD_ORDER_RETURN_ITEM","ADD ORDER RETURN ITEM WITH SUPPLIER PRODUCT "+newOrderReturnItem.getSupplierProduct().getSerial()+" WITH "+newOrderReturnItem.getQuantity()+" UNITS.",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(Long orderId, String supplierProductSerial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderReturn orderReturn;
            SupplierProduct supplierProduct;
            OrderReturnItem orderReturnItem;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                orderReturn = orderReturnRepository.findByOrderId(orderId);
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(orderReturn==null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturn);
            }else{
                orderReturnItem = orderReturnItemRepository.findBySupplierProductIdAndOrderReturnIdAndStatusTrue(supplierProduct.getId(), orderReturn.getId());
            }
            try{
                orderReturnItem.setStatus(false);
                orderReturnItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderReturnItem.setTokenUser(user.getUsername());
                orderReturnItemRepository.save(orderReturnItem);
                iGeneralStock.out(supplierProduct.getSerial(), orderReturnItem.getQuantity(), user.getUsername());
                iWarehouseStock.out(orderReturn.getOrderStock().getWarehouse(),supplierProduct, orderReturnItem.getQuantity(), user);
                iAudit.save("DELETE_ORDER_RETURN_ITEM","DELETE ORDER RETURN ITEM WITH SUPPLIER PRODUCT "+orderReturnItem.getSupplierProduct().getSerial()+" WITH "+orderReturnItem.getQuantity()+" UNITS.",user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(Long orderId, String supplierProductSerial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderReturn orderReturn;
            SupplierProduct supplierProduct;
            OrderReturnItem orderReturnItem;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                orderReturn = orderReturnRepository.findByOrderId(orderId);
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(orderReturn==null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturn);
            }else{
                orderReturnItem = orderReturnItemRepository.findBySupplierProductIdAndOrderReturnIdAndStatusFalse(supplierProduct.getId(), orderReturn.getId());
            }
            try{
                orderReturnItem.setStatus(true);
                orderReturnItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderReturnItem.setTokenUser(user.getUsername());
                orderReturnItemRepository.save(orderReturnItem);
                iGeneralStock.in(supplierProduct.getSerial(), orderReturnItem.getQuantity(), user.getUsername());
                iWarehouseStock.in(orderReturn.getOrderStock().getWarehouse(),supplierProduct, orderReturnItem.getQuantity(), user);
                iAudit.save("ACTIVATE_ORDER_RETURN_ITEM","ACTIVATE ORDER RETURN ITEM WITH SUPPLIER PRODUCT "+orderReturnItem.getSupplierProduct().getSerial()+" WITH "+orderReturnItem.getQuantity()+" UNITS.",user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> update(Long orderId, String supplierProductSerial, Integer quantity, String tokenUser) throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderReturn orderReturn;
            SupplierProduct supplierProduct;
            OrderReturnItem orderReturnItem;
            GeneralStock generalStock;
            WarehouseStock warehouseStock;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                orderReturn = orderReturnRepository.findByOrderId(orderId);
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(orderReturn==null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturn);
            }else{
                orderReturnItem = orderReturnItemRepository.findBySupplierProductIdAndOrderReturnIdAndStatusTrue(supplierProduct.getId(), orderReturn.getId());
            }
            if(orderReturnItem==null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnItem);
            }else{
                generalStock = generalStockRepository.findByClientIdAndSupplierProductId(user.getClientId(),supplierProduct.getId());
                warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(orderReturn.getOrderStock().getWarehouseId(),supplierProduct.getId());
            }
            if(Objects.equals(quantity, orderReturnItem.getQuantity())){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnItemQuantityNotChange);
            }
            try{
                if(quantity>orderReturnItem.getQuantity()){
                    generalStock.setQuantity(generalStock.getQuantity()+(quantity-orderReturnItem.getQuantity()));
                    warehouseStock.setQuantity(warehouseStock.getQuantity()+(quantity-orderReturnItem.getQuantity()));
                }
                if(quantity<orderReturnItem.getQuantity()){
                    generalStock.setQuantity(generalStock.getQuantity()-(orderReturnItem.getQuantity()-quantity));
                    warehouseStock.setQuantity(warehouseStock.getQuantity()+(orderReturnItem.getQuantity()-quantity));
                }
                orderReturnItem.setQuantity(quantity);
                orderReturnItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderReturnItem.setTokenUser(user.getUsername());
                orderReturnItemRepository.save(orderReturnItem);
                iAudit.save("UPDATE_ORDER_RETURN_ITEM","UPDATE ORDER RETURN ITEM WITH SUPPLIER PRODUCT "+orderReturnItem.getSupplierProduct().getSerial()+" WITH "+orderReturnItem.getQuantity()+" UNITS.",user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<OrderReturnItemDTO>> list(String user, Long orderId) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            List<OrderReturnItem> orderReturnItemList;
            OrderReturn orderReturn;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(orderId!=null){
                    orderReturn = orderReturnRepository.findByOrderId(orderId);
                }else{
                    orderReturn = null;
                }
                if(orderReturn!=null){
                    orderReturnItemList = orderReturnItemRepository.findAllByClientIdAndOrderReturnIdAndStatusTrue(clientId,orderId);
                }else{
                    orderReturnItemList = orderReturnItemRepository.findAllByClientIdAndStatusTrue(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderReturnItemList.isEmpty()){
                return Collections.emptyList();
            }
            return orderReturnItemList.stream().map(orderReturnItem -> OrderReturnItemDTO.builder()
                    .orderId(orderReturnItem.getOrderReturn().getOrderId())
                    .product(orderReturnItem.getProduct().getSku())
                    .supplierProduct(orderReturnItem.getSupplierProduct().getSerial())
                    .returnType(orderReturnItem.getOrderReturnType().getName())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .updateDate(new Date(System.currentTimeMillis()))
                    .quantity(orderReturnItem.getQuantity())
                    .warehouse(orderReturnItem.getOrderReturn().getOrderStock().getWarehouse().getName())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<OrderReturnItemDTO>> listPagination(
            String user,
            List<Long> orders,
            List<String> products,
            List<String> supplierProducts,
            List<String> warehouses,
            List<String> orderReturnTypes,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderReturnItem> orderReturnItemPage;
            Long clientId;
            List<Long> orderIds;
            List<Long> productIds;
            List<Long> supplierProductIds;
            List<Long> warehouseIds;
            List<Long> orderReturnTypeIds;
            if(orders != null && !orders.isEmpty()){
                orderIds = orders;
            }else{
                orderIds = new ArrayList<>();
            }
            if(products != null && !products.isEmpty()){
                productIds = productRepository.findBySkuIn(
                        products.stream().map(String::toUpperCase).toList()
                ).stream().map(Product::getId).toList();
            }else {
                productIds = new ArrayList<>();
            }
            if(supplierProducts != null && !supplierProducts.isEmpty()){
                supplierProductIds = supplierProductRepository.findBySerialIn(
                        supplierProducts.stream().map(String::toUpperCase).toList()
                ).stream().map(SupplierProduct::getId).toList();
            }else{
                supplierProductIds = new ArrayList<>();
            }
            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }
            if(orderReturnTypes!=null && !orderReturnTypes.isEmpty()){
                orderReturnTypeIds = orderReturnTypeRepository.findByNameIn(
                        orderReturnTypes.stream().map(String::toUpperCase).toList()
                ).stream().map(OrderReturnType::getId).toList();
            }else{
                orderReturnTypeIds = new ArrayList<>();
            }
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderReturnItemPage = orderReturnItemRepositoryCustom.searchForOrderReturnItem(
                        clientId,
                        orderIds,
                        productIds,
                        supplierProductIds,
                        warehouseIds,
                        orderReturnTypeIds,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderReturnItemPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<OrderReturnItemDTO> orderReturnItemDTOS = orderReturnItemPage.getContent().stream().map(orderReturnItem -> OrderReturnItemDTO.builder()
                    .orderId(orderReturnItem.getOrderReturn().getOrderId())
                    .product(orderReturnItem.getProduct().getSku())
                    .supplierProduct(orderReturnItem.getSupplierProduct().getSerial())
                    .warehouse(orderReturnItem.getOrderReturn().getOrderStock().getWarehouse().getName())
                    .returnType(orderReturnItem.getOrderReturnType().getName())
                    .quantity(orderReturnItem.getQuantity())
                    .registrationDate(orderReturnItem.getRegistrationDate())
                    .updateDate(orderReturnItem.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(orderReturnItemDTOS,orderReturnItemPage.getPageable(),orderReturnItemPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<OrderReturnItemDTO>> listFalse(
            String user,
            List<Long> orders,
            List<String> products,
            List<String> supplierProducts,
            List<String> warehouses,
            List<String> orderReturnTypes,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderReturnItem> orderReturnItemPage;
            Long clientId;
            List<Long> orderIds;
            List<Long> productIds;
            List<Long> supplierProductIds;
            List<Long> warehouseIds;
            List<Long> orderReturnTypeIds;
            if(orders != null && !orders.isEmpty()){
                orderIds = orders;
            }else{
                orderIds = new ArrayList<>();
            }
            if(products != null && !products.isEmpty()){
                productIds = productRepository.findBySkuIn(
                        products.stream().map(String::toUpperCase).toList()
                ).stream().map(Product::getId).toList();
            }else {
                productIds = new ArrayList<>();
            }
            if(supplierProducts != null && !supplierProducts.isEmpty()){
                supplierProductIds = supplierProductRepository.findBySerialIn(
                        supplierProducts.stream().map(String::toUpperCase).toList()
                ).stream().map(SupplierProduct::getId).toList();
            }else{
                supplierProductIds = new ArrayList<>();
            }
            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }
            if(orderReturnTypes!=null && !orderReturnTypes.isEmpty()){
                orderReturnTypeIds = orderReturnTypeRepository.findByNameIn(
                        orderReturnTypes.stream().map(String::toUpperCase).toList()
                ).stream().map(OrderReturnType::getId).toList();
            }else{
                orderReturnTypeIds = new ArrayList<>();
            }
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderReturnItemPage = orderReturnItemRepositoryCustom.searchForOrderReturnItem(
                        clientId,
                        orderIds,
                        productIds,
                        supplierProductIds,
                        warehouseIds,
                        orderReturnTypeIds,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderReturnItemPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<OrderReturnItemDTO> orderReturnItemDTOS = orderReturnItemPage.getContent().stream().map(orderReturnItem -> OrderReturnItemDTO.builder()
                    .orderId(orderReturnItem.getOrderReturn().getOrderId())
                    .product(orderReturnItem.getProduct().getSku())
                    .supplierProduct(orderReturnItem.getSupplierProduct().getSerial())
                    .warehouse(orderReturnItem.getOrderReturn().getOrderStock().getWarehouse().getName())
                    .returnType(orderReturnItem.getOrderReturnType().getName())
                    .quantity(orderReturnItem.getQuantity())
                    .registrationDate(orderReturnItem.getRegistrationDate())
                    .updateDate(orderReturnItem.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(orderReturnItemDTOS,orderReturnItemPage.getPageable(),orderReturnItemPage.getTotalElements());
        });
    }
}
