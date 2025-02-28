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
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
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
    private final IUtil iUtil;
    @Override
    public CompletableFuture<ResponseSuccess> save(UUID orderId, RequestOrderStockItem requestOrderStockItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderItem orderItem;
            SupplierProduct supplierProduct;
            OrderStock orderStock;
            Product product;
            OrderStockItem orderStockItem;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(requestOrderStockItem.getProductId());
                ordering = orderingRepository.findById(orderId).orElse(null);
                supplierProduct = supplierProductRepository.findByIdAndStatusTrue(requestOrderStockItem.getSupplierProductId());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderStock = orderStockRepository.findByOrderIdAndClientId(orderId,user.getClientId());
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
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
            }else{
                orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
            }

            if(!Objects.equals(iUtil.buildProductSku(supplierProduct.getProduct()), iUtil.buildProductSku(orderItem.getProduct()))){
                throw new BadRequestExceptions(Constants.ErrorOrderStockProduct);
            }

            if(requestOrderStockItem.getQuantity() > orderItem.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItemQuantity);
            }

            if(orderStockItem!=null){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItemExists);
            }

            try{
                OrderStockItem newOrderStockItem = orderStockItemRepository.save(OrderStockItem.builder()
                        .orderStock(orderStock)
                        .orderStockId(orderStock.getId())
                        .orderItem(orderItem)
                        .orderId(ordering.getId())
                        .ordering(ordering)
                        .orderItemId(orderItem.getId())
                        .registrationDate(OffsetDateTime.now())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .status(true)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .updateDate(OffsetDateTime.now())
                        .quantity(requestOrderStockItem.getQuantity())
                        .user(user)
                        .userId(user.getId())
                        .build());
                iAudit.save(
                        "ADD_ORDER_STOCK_ITEM",
                        "PRODUCTO DE PREPARACION DE PEDIDO CON PRODUCTO DE INVENTARIO "+
                                iUtil.buildInventorySku(newOrderStockItem.getSupplierProduct())+
                                " CON "+newOrderStockItem.getQuantity()+" UNIDADES CREADO.",
                        newOrderStockItem.getOrderStock().getOrderId().toString(),user.getUsername());
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
            UUID orderId,
            List<String> warehouses,
            String productSku,
            String serial,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderStockItem> pageOrderStock;
            UUID clientId;
            List<UUID> warehouseIds;

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageOrderStock = orderStockItemRepositoryCustom.searchForOrderStockItem(
                        clientId,
                        orderId,
                        warehouseIds,
                        productSku,
                        serial,
                        model,
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
                    .supplierProduct(iUtil.buildInventorySku(orderStockItem.getSupplierProduct()))
                    .product(iUtil.buildProductSku(orderStockItem.getSupplierProduct().getProduct()))
                    .color(orderStockItem.getSupplierProduct().getProduct().getColor().getName())
                    .model(orderStockItem.getSupplierProduct().getProduct().getModel().getName())
                    .size(orderStockItem.getSupplierProduct().getProduct().getSize().getName())
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
            UUID orderId,
            List<String> warehouses,
            String productSku,
            String serial,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderStockItem> pageOrderStock;
            UUID clientId;
            List<UUID> warehouseIds;

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageOrderStock = orderStockItemRepositoryCustom.searchForOrderStockItem(
                        clientId,
                        orderId,
                        warehouseIds,
                        productSku,
                        serial,
                        model,
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
                    .product(iUtil.buildProductSku(orderStockItem.getSupplierProduct().getProduct()))
                    .supplierProduct(iUtil.buildInventorySku(orderStockItem.getSupplierProduct()))
                    .color(orderStockItem.getSupplierProduct().getProduct().getColor().getName())
                    .model(orderStockItem.getSupplierProduct().getProduct().getModel().getName())
                    .size(orderStockItem.getSupplierProduct().getProduct().getSize().getName())
                    .quantity(orderStockItem.getQuantity())
                    .registrationDate(orderStockItem.getRegistrationDate())
                    .updateDate(orderStockItem.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(orderStockItemDTOList,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Boolean> checkWarehouseItemStock(UUID orderId,Warehouse warehouse,RequestOrderStockItem requestOrderStockItem) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            SupplierProduct supplierProduct;
            OrderItem orderItem;
            Product product;
            try{
                supplierProduct = supplierProductRepository.findByIdAndStatusTrue(requestOrderStockItem.getSupplierProductId());
                product = productRepository.findByIdAndStatusTrue(requestOrderStockItem.getProductId());
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
    public CompletableFuture<List<OrderStockItemDTO>> listOrderStockItem(String user,UUID orderId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
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
                    .product(iUtil.buildProductSku(orderStockItem.getSupplierProduct().getProduct()))
                    .supplierProduct(iUtil.buildInventorySku(orderStockItem.getSupplierProduct()))
                    .quantity(orderStockItem.getQuantity())
                    .registrationDate(orderStockItem.getRegistrationDate())
                    .updateDate(orderStockItem.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<OrderStockItemDTO>> listOrderStockItemFalse(String user,UUID orderId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
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
                    .product(iUtil.buildProductSku(orderStockItem.getSupplierProduct().getProduct()))
                    .supplierProduct(iUtil.buildInventorySku(orderStockItem.getSupplierProduct()))
                    .quantity(orderStockItem.getQuantity())
                    .registrationDate(orderStockItem.getRegistrationDate())
                    .updateDate(orderStockItem.getUpdateDate())
                    .product(iUtil.buildProductSku(orderStockItem.getOrderItem().getProduct()))
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(UUID orderId, UUID supplierProductId, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            UUID clientId;
            SupplierProduct supplierProduct;
            OrderStock orderStock;
            OrderStockItem orderStockItem;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findByIdAndStatusTrue(supplierProductId);

            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                clientId = user.getClientId();
                orderStock = orderStockRepository.findByOrderIdAndClientId(orderId,user.getClientId());
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
                orderStockItem.setUpdateDate(OffsetDateTime.now());
                orderStockItem.setUser(user);
                orderStockItem.setUserId(user.getId());
                orderStockItemRepository.save(orderStockItem);
                iAudit.save(
                        "DELETE_ORDER_STOCK_ITEM",
                        "PRODUCTO DE PREPARACION DE PEDIDO CON PRODUCTO DE INVENTARIO "+
                                iUtil.buildInventorySku(orderStockItem.getSupplierProduct())+" CON "+
                                orderStockItem.getQuantity()+" UNIDADES DESACTIVADO.",
                        orderStockItem.getOrderStock().getOrderId().toString(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(UUID orderId, UUID supplierProductId, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            User user;
            SupplierProduct supplierProduct;
            OrderStock orderStock;
            OrderStockItem orderStockItem;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findByIdAndStatusTrue(supplierProductId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                clientId=user.getClientId();
                orderStock = orderStockRepository.findByOrderIdAndClientId(
                        orderId,
                        user.getClientId()
                );
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
                orderStockItem.setUpdateDate(OffsetDateTime.now());
                orderStockItem.setUserId(user.getId());
                orderStockItem.setUser(user);
                orderStockItemRepository.save(orderStockItem);
                iAudit.save(
                        "ACTIVATE_ORDER_STOCK_ITEM",
                        "PRODUCTO DE PREPARACION DE PEDIDO CON PRODUCTO DE INVENTARIO "+
                                iUtil.buildInventorySku(orderStockItem.getSupplierProduct())+
                                " CON "+orderStockItem.getQuantity()+" UNIDADES ACTIVADO.",
                        orderStockItem.getOrderStock().getOrderId().toString(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> update(UUID orderId, UUID supplierProductId, String tokenUser, Integer quantity) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            User user;
            SupplierProduct supplierProduct;
            OrderStock orderStock;
            OrderStockItem orderStockItem;
            WarehouseStock warehouseStock;
            Integer newQuantity;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findByIdAndStatusTrue(supplierProductId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                clientId = user.getClientId();
                orderStock = orderStockRepository.findByOrderIdAndClientId(orderId,user.getClientId());
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
                newQuantity = orderStockItem.getQuantity() + quantity;
            }
            if(orderStockItem.getOrderItem().getQuantity() < newQuantity){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItemUpdateOrderQuantity);
            }
            if(warehouseStock.getQuantity() < newQuantity){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItemUpdateStockQuantity);
            }

            try {
                orderStockItem.setQuantity(newQuantity);
                orderStockItem.setUpdateDate(OffsetDateTime.now());
                orderStockItem.setUser(user);
                orderStockItem.setUserId(user.getId());
                orderStockItemRepository.save(orderStockItem);
                iAudit.save(
                        "UPDATE_ORDER_STOCK_ITEM",
                        "PRODUCTO DE PREPARACION DE PEDIDO CON PRODUCTO DE INVENTARIO "+
                                iUtil.buildInventorySku(orderStockItem.getSupplierProduct())+" CON "+
                                orderStockItem.getQuantity()+" UNIDADES ACTUALIZADO.",
                        orderStockItem.getOrderStock().getOrderId().toString(),user.getUsername());
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
