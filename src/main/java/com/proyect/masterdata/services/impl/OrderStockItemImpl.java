package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IOrderStockItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;

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
    @Override
    public ResponseSuccess save(OrderStock orderStock, RequestOrderStockItem requestOrderStockItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Ordering ordering;
        OrderItem orderItem;
        SupplierProduct supplierProduct;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            ordering = orderingRepository.findById(orderStock.getOrderId()).orElse(null);
            orderItem = orderItemRepository.findByIdAndOrderId(requestOrderStockItem.getItemId(), orderStock.getOrderId());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderStockItem.getSupplierProductSerial().toUpperCase());
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

        if(orderItem == null){
            throw new BadRequestExceptions(Constants.ErrorItem);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        try{
            orderStockItemRepository.save(OrderStockItem.builder()
                            .orderStock(orderStock)
                            .orderStockId(orderStock.getId())
                            .orderItem(orderItem)
                            .orderId(ordering.getId())
                            .ordering(ordering)
                            .itemId(orderItem.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .status(true)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .updateDate(new Date(System.currentTimeMillis()))
                            .quantity(requestOrderStockItem.getQuantity())
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<OrderStockItemDTO> list(String user, Long orderId, String supplierProductSerial, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<OrderStockItem> pageOrderStock;
        Long clientId;
        Long supplierProductId;

        if (supplierProductSerial != null) {
            supplierProductId = supplierProductRepository.findBySerial(supplierProductSerial.toUpperCase()).getId();
        } else {
            supplierProductId = null;
        }

        try{
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageOrderStock = orderStockItemRepositoryCustom.searchForOrderStockItem(clientId,orderId,supplierProductId,sort,sortColumn,pageNumber,pageSize,true);
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
                    .itemId(orderStockItem.getItemId())
                    .supplierProductSerial(orderStockItem.getSupplierProduct().getSerial())
                    .quantity(orderStockItem.getQuantity())
                    .registrationDate(orderStockItem.getRegistrationDate())
                    .updateDate(orderStockItem.getUpdateDate())
                    .build()).toList();

        return new PageImpl<>(orderStockItemDTOList,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
    }

    @Override
    public Page<OrderStockItemDTO> listFalse(String user, Long orderId, String supplierProductSerial, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<OrderStockItem> pageOrderStock;
        Long clientId;
        Long supplierProductId;

        if (supplierProductSerial != null) {
            supplierProductId = supplierProductRepository.findBySerial(supplierProductSerial.toUpperCase()).getId();
        } else {
            supplierProductId = null;
        }

        try{
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageOrderStock = orderStockItemRepositoryCustom.searchForOrderStockItem(clientId,orderId,supplierProductId,sort,sortColumn,pageNumber,pageSize,false);
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
                .itemId(orderStockItem.getItemId())
                .supplierProductSerial(orderStockItem.getSupplierProduct().getSerial())
                .quantity(orderStockItem.getQuantity())
                .registrationDate(orderStockItem.getRegistrationDate())
                .updateDate(orderStockItem.getUpdateDate())
                .build()).toList();

        return new PageImpl<>(orderStockItemDTOList,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
    }

    @Override
    public Boolean checkWarehouseItemStock(Long orderId,Warehouse warehouse,RequestOrderStockItem requestOrderStockItem) throws InternalErrorExceptions, BadRequestExceptions {
        SupplierProduct supplierProduct;
        OrderItem orderItem;
        try{
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderStockItem.getSupplierProductSerial().toUpperCase());
            orderItem = orderItemRepository.findByIdAndOrderId(requestOrderStockItem.getItemId(), orderId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        if(orderItem == null){
            throw new BadRequestExceptions(Constants.ErrorItem);
        }

        try{
            WarehouseStock warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),supplierProduct.getId());
            return warehouseStock.getQuantity() >= orderItem.getQuantity();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<OrderStockItemDTO> listOrderStockItem(String user) throws BadRequestExceptions, InternalErrorExceptions {
        Long clientId;
        List<OrderStockItem> orderStockItems;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            orderStockItems = orderStockItemRepository.findAllByClientIdAndStatusTrue(clientId);
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
                .itemId(orderStockItem.getItemId())
                .supplierProductSerial(orderStockItem.getSupplierProduct().getSerial())
                .quantity(orderStockItem.getQuantity())
                .registrationDate(orderStockItem.getRegistrationDate())
                .updateDate(orderStockItem.getUpdateDate())
                .build()).toList();
    }

    @Override
    public List<OrderStockItemDTO> listOrderStockItemFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        Long clientId;
        List<OrderStockItem> orderStockItems;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            orderStockItems = orderStockItemRepository.findAllByClientIdAndStatusFalse(clientId);
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
                .itemId(orderStockItem.getItemId())
                .supplierProductSerial(orderStockItem.getSupplierProduct().getSerial())
                .quantity(orderStockItem.getQuantity())
                .registrationDate(orderStockItem.getRegistrationDate())
                .updateDate(orderStockItem.getUpdateDate())
                .build()).toList();
    }
}
