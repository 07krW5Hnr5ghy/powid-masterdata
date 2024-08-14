package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderStockItem;
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
public class OrderStockItemImpl implements IOrderStockItem {
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final OrderStockItemRepository orderStockItemRepository;
    private final OrderItemRepository orderItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final OrderStockItemRepositoryCustom orderStockItemRepositoryCustom;
    private final WarehouseStockRepository warehouseStockRepository;
    private final ProductRepository productRepository;
    private final OrderStockRepository orderStockRepository;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(Long orderId, RequestOrderStockItem requestOrderStockItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderItem orderItem;
            SupplierProduct supplierProduct;
            OrderStock orderStock;
            Product product;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(requestOrderStockItem.getProduct().toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderStockItem.getSupplierProduct().toUpperCase());
                orderStock = orderStockRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else{
                orderItem = orderItemRepository.findByProductIdAndOrderId(product.getId(), orderId);
            }

            if(orderItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            if(!Objects.equals(supplierProduct.getProduct().getSku(), orderItem.getProduct().getSku())){
                throw new BadRequestExceptions(Constants.ErrorOrderStockProduct);
            }

            if(requestOrderStockItem.getQuantity() > orderItem.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItemQuantity);
            }

            try{
                OrderStockItem newOrderStockItem = orderStockItemRepository.save(OrderStockItem.builder()
                        .orderStock(orderStock)
                        .orderStockId(orderStock.getId())
                        .orderItem(orderItem)
                        .orderId(ordering.getId())
                        .ordering(ordering)
                        .orderItemId(orderItem.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .status(true)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .updateDate(new Date(System.currentTimeMillis()))
                        .quantity(requestOrderStockItem.getQuantity())
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_ORDER_STOCK_ITEM","ADD ORDER STOCK ITEM WITH SUPPLIER PRODUCT "+newOrderStockItem.getSupplierProduct().getSerial()+" WITH "+newOrderStockItem.getQuantity()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<OrderStockItemDTO>> list(
            String user,
            List<Long> orders,
            List<String> warehouses,
            List<String> products,
            List<String> supplierProducts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderStockItem> pageOrderStock;
            Long clientId;
            List<Long> orderIds;
            List<Long> warehouseIds;
            List<Long> productIds;
            List<Long> supplierProductIds;

            if(orders != null && !orders.isEmpty()){
                orderIds = orders;
            }else {
                orderIds = new ArrayList<>();
            }

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            if(products != null && !products.isEmpty()){
                productIds = productRepository.findBySkuIn(
                        products.stream().map(String::toUpperCase).toList()
                ).stream().map(Product::getId).toList();
            }else{
                productIds = new ArrayList<>();
            }

            if(supplierProducts != null && !supplierProducts.isEmpty()){
                supplierProductIds = supplierProductRepository.findBySerialIn(
                        supplierProducts.stream().map(String::toUpperCase).toList()
                ).stream().map(SupplierProduct::getId).toList();
            }else {
                supplierProductIds = new ArrayList<>();
            }

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageOrderStock = orderStockItemRepositoryCustom.searchForOrderStockItem(
                        clientId,
                        orderIds,
                        warehouseIds,
                        productIds,
                        supplierProductIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageOrderStock.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<OrderStockItemDTO> orderStockItemDTOList = pageOrderStock.getContent().stream().map(orderStockItem -> OrderStockItemDTO.builder()
                    .orderId(orderStockItem.getOrderStock().getOrderId())
                    .warehouse(orderStockItem.getOrderStock().getWarehouse().getName())
                    .supplierProduct(orderStockItem.getSupplierProduct().getSerial())
                    .product(orderStockItem.getSupplierProduct().getProduct().getSku())
                    .quantity(orderStockItem.getQuantity())
                    .registrationDate(orderStockItem.getRegistrationDate())
                    .updateDate(orderStockItem.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(orderStockItemDTOList,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<OrderStockItemDTO>> listFalse(
            String user,
            List<Long> orders,
            List<String> warehouses,
            List<String> products,
            List<String> supplierProducts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderStockItem> pageOrderStock;
            Long clientId;
            List<Long> orderIds;
            List<Long> warehouseIds;
            List<Long> productIds;
            List<Long> supplierProductIds;

            if(orders != null && !orders.isEmpty()){
                orderIds = orders;
            }else {
                orderIds = new ArrayList<>();
            }

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            if(products != null && !products.isEmpty()){
                productIds = productRepository.findBySkuIn(
                        products.stream().map(String::toUpperCase).toList()
                ).stream().map(Product::getId).toList();
            }else{
                productIds = new ArrayList<>();
            }

            if(supplierProducts != null && !supplierProducts.isEmpty()){
                supplierProductIds = supplierProductRepository.findBySerialIn(
                        supplierProducts.stream().map(String::toUpperCase).toList()
                ).stream().map(SupplierProduct::getId).toList();
            }else {
                supplierProductIds = new ArrayList<>();
            }

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageOrderStock = orderStockItemRepositoryCustom.searchForOrderStockItem(
                        clientId,
                        orderIds,
                        warehouseIds,
                        productIds,
                        supplierProductIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageOrderStock.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<OrderStockItemDTO> orderStockItemDTOList = pageOrderStock.getContent().stream().map(orderStockItem -> OrderStockItemDTO.builder()
                    .orderId(orderStockItem.getOrderStock().getOrderId())
                    .warehouse(orderStockItem.getOrderStock().getWarehouse().getName())
                    .product(orderStockItem.getSupplierProduct().getProduct().getSku())
                    .supplierProduct(orderStockItem.getSupplierProduct().getSerial())
                    .quantity(orderStockItem.getQuantity())
                    .registrationDate(orderStockItem.getRegistrationDate())
                    .updateDate(orderStockItem.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(orderStockItemDTOList,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Boolean> checkWarehouseItemStock(Long orderId,Warehouse warehouse,RequestOrderStockItem requestOrderStockItem) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            SupplierProduct supplierProduct;
            OrderItem orderItem;
            Product product;
            try{
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderStockItem.getSupplierProduct().toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(requestOrderStockItem.getProduct().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else{
                orderItem = orderItemRepository.findByProductIdAndOrderId(product.getId(), orderId);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            if(orderItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }

            try{
                WarehouseStock warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),supplierProduct.getId());
                return warehouseStock.getQuantity() >= orderItem.getQuantity();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<OrderStockItemDTO>> listOrderStockItem(String user,Long orderId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            List<OrderStockItem> orderStockItems;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(orderId != null){
                    orderStockItems = orderStockItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(clientId,orderId);
                }else{
                    orderStockItems = orderStockItemRepository.findAllByClientIdAndStatusTrue(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(orderStockItems.isEmpty()){
                return Collections.emptyList();
            }

            return orderStockItems.stream().map(orderStockItem -> OrderStockItemDTO.builder()
                    .orderId(orderStockItem.getOrderStock().getOrderId())
                    .warehouse(orderStockItem.getOrderStock().getWarehouse().getName())
                    .product(orderStockItem.getSupplierProduct().getProduct().getSku())
                    .supplierProduct(orderStockItem.getSupplierProduct().getSerial())
                    .quantity(orderStockItem.getQuantity())
                    .registrationDate(orderStockItem.getRegistrationDate())
                    .updateDate(orderStockItem.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<OrderStockItemDTO>> listOrderStockItemFalse(String user,Long orderId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            List<OrderStockItem> orderStockItems;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(orderId != null){
                    orderStockItems = orderStockItemRepository.findAllByClientIdAndOrderIdAndStatusFalse(clientId,orderId);
                }else{
                    orderStockItems = orderStockItemRepository.findAllByClientIdAndStatusFalse(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(orderStockItems.isEmpty()){
                return Collections.emptyList();
            }

            return orderStockItems.stream().map(orderStockItem -> OrderStockItemDTO.builder()
                    .orderId(orderStockItem.getOrderStock().getOrderId())
                    .warehouse(orderStockItem.getOrderStock().getWarehouse().getName())
                    .product(orderStockItem.getSupplierProduct().getProduct().getSku())
                    .supplierProduct(orderStockItem.getSupplierProduct().getSerial())
                    .quantity(orderStockItem.getQuantity())
                    .registrationDate(orderStockItem.getRegistrationDate())
                    .updateDate(orderStockItem.getUpdateDate())
                    .product(orderStockItem.getOrderItem().getProduct().getSku())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(Long orderId, String supplierProductSerial, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Long clientId;
            SupplierProduct supplierProduct;
            OrderStock orderStock;
            OrderStockItem orderStockItem;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
                orderStock = orderStockRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                clientId = user.getClientId();
            }
            if(clientId == null){
                throw new BadRequestExceptions(Constants.ErrorClient);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(orderStock == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }else{
                orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
            }
            if(orderStockItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItem);
            }
            try{
                orderStockItem.setStatus(false);
                orderStockItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderStockItem.setTokenUser(user.getUsername());
                orderStockItemRepository.save(orderStockItem);
                iAudit.save("DELETE_ORDER_STOCK_ITEM","DELETE ORDER STOCK ITEM WITH SUPPLIER PRODUCT "+orderStockItem.getSupplierProduct().getSerial()+" WITH "+orderStockItem.getQuantity()+".",user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(Long orderId, String supplierProductSerial, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            User user;
            SupplierProduct supplierProduct;
            OrderStock orderStock;
            OrderStockItem orderStockItem;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
                orderStock = orderStockRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                clientId=user.getClientId();
            }
            if(clientId == null){
                throw new BadRequestExceptions(Constants.ErrorClient);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(orderStock == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }else{
                orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
            }
            if(orderStockItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItem);
            }
            try{
                orderStockItem.setStatus(true);
                orderStockItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderStockItem.setTokenUser(user.getUsername());
                orderStockItemRepository.save(orderStockItem);
                iAudit.save("ACTIVATE_ORDER_STOCK_ITEM","ACTIVATE ORDER STOCK ITEM WITH SUPPLIER PRODUCT "+orderStockItem.getSupplierProduct().getSerial()+" WITH "+orderStockItem.getQuantity()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> update(Long orderId, String supplierProductSerial, String tokenUser, Integer quantity) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            User user;
            SupplierProduct supplierProduct;
            OrderStock orderStock;
            OrderStockItem orderStockItem;
            WarehouseStock warehouseStock;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
                orderStock = orderStockRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                clientId = user.getClientId();
            }
            if(clientId==null){
                throw new BadRequestExceptions(Constants.ErrorClient);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(orderStock == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }else{
                orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
            }
            if(orderStockItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItem);
            }else {
                warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(orderStock.getWarehouseId(),supplierProduct.getId());
            }
            if(orderStockItem.getOrderItem().getQuantity() < quantity){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItemUpdateOrderQuantity);
            }
            if(warehouseStock.getQuantity() < quantity){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItemUpdateStockQuantity);
            }

            try {
                orderStockItem.setQuantity(quantity);
                orderStockItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderStockItem.setTokenUser(user.getUsername());
                orderStockItemRepository.save(orderStockItem);
                iAudit.save("UPDATE_ORDER_STOCK_ITEM","UPDATE ORDER STOCK ITEM WITH SUPPLIER PRODUCT "+orderStockItem.getSupplierProduct().getSerial()+" WITH "+orderStockItem.getQuantity()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
