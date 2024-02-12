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
    private final ItemRepository itemRepository;
    private final WarehouseRepository warehouseRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final OrderStockItemRepositoryCustom orderStockItemRepositoryCustom;
    private final WarehouseStockRepository warehouseStockRepository;
    private final ProductRepository productRepository;
    @Override
    public ResponseSuccess save(OrderStock orderStock, RequestOrderStockItem requestOrderStockItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Ordering ordering;
        Item item;
        SupplierProduct supplierProduct;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            ordering = orderingRepository.findById(orderStock.getOrderId()).orElse(null);
            item = itemRepository.findByIdAndOrderId(requestOrderStockItem.getItemId(), orderStock.getOrderId());
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

        if(item == null){
            throw new BadRequestExceptions(Constants.ErrorItem);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        try{
            orderStockItemRepository.save(OrderStockItem.builder()
                            .orderStock(orderStock)
                            .orderStockId(orderStock.getId())
                            .item(item)
                            .itemId(item.getId())
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
    public Page<OrderStockItemDTO> list(String warehouse, Long orderId, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<OrderStockItem> pageOrderStock;
        User userdata;
        Warehouse warehouseData;

        if (user != null) {
            userdata = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
        } else {
            userdata = null;
        }

        if (warehouse != null) {
            warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
        } else {
            warehouseData = null;
        }

        try{
            pageOrderStock = orderStockItemRepositoryCustom.searchForOrderStock(warehouseData,orderId,userdata,true,sort,sortColumn,pageNumber,pageSize);
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
                    .serialSupplierProduct(orderStockItem.getSupplierProduct().getSerial())
                    .quantity(orderStockItem.getQuantity())
                    .registrationDate(orderStockItem.getRegistrationDate())
                    .updateDate(orderStockItem.getUpdateDate())
                    .build()).toList();

        return new PageImpl<>(orderStockItemDTOList,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
    }

    @Override
    public Page<OrderStockItemDTO> listFalse(String warehouse, Long orderId, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<OrderStockItem> pageOrderStock;
        User userdata;
        Warehouse warehouseData;

        if (user != null) {
            userdata = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
        } else {
            userdata = null;
        }

        if (warehouse != null) {
            warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
        } else {
            warehouseData = null;
        }

        try{
            pageOrderStock = orderStockItemRepositoryCustom.searchForOrderStock(warehouseData,orderId,userdata,false,sort,sortColumn,pageNumber,pageSize);
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
                .serialSupplierProduct(orderStockItem.getSupplierProduct().getSerial())
                .quantity(orderStockItem.getQuantity())
                .registrationDate(orderStockItem.getRegistrationDate())
                .updateDate(orderStockItem.getUpdateDate())
                .build()).toList();

        return new PageImpl<>(orderStockItemDTOList,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
    }

    @Override
    public Boolean checkWarehouseItemStock(Long orderId,Warehouse warehouse,RequestOrderStockItem requestOrderStockItem) throws InternalErrorExceptions, BadRequestExceptions {
        SupplierProduct supplierProduct;
        Item item;
        try{
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderStockItem.getSupplierProductSerial().toUpperCase());
            item = itemRepository.findByIdAndOrderId(requestOrderStockItem.getItemId(), orderId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        if(item == null){
            throw new BadRequestExceptions(Constants.ErrorItem);
        }

        try{
            WarehouseStock warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),supplierProduct.getId());
            return warehouseStock.getQuantity() >= item.getQuantity();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
