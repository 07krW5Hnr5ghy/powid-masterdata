package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderStockDTO;
import com.proyect.masterdata.dto.request.RequestOrderStock;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IOrderStock;
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
public class OrderStockImpl implements IOrderStock {
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final OrderStockRepository orderStockRepository;
    private final ItemRepository itemRepository;
    private final WarehouseRepository warehouseRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final OrderStockRepositoryCustom orderStockRepositoryCustom;
    private final ProductRepository productRepository;
    @Override
    public ResponseSuccess save(Long orderId, List<RequestOrderStock> requestOrderStocks, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Ordering ordering;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            ordering = orderingRepository.findById(orderId).orElse(null);
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

        try{
            for (RequestOrderStock requestOrderStock : requestOrderStocks){
                Item item = itemRepository.findByIdAndOrderId(requestOrderStock.getItemId(), ordering.getId());
                Warehouse warehouse = warehouseRepository.findByNameAndStatusTrue(requestOrderStock.getWarehouse().toUpperCase());
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderStock.getSupplierProductSerial());
                orderStockRepository.save(OrderStock.builder()
                                .quantity(requestOrderStock.getQuantity())
                                .orderId(ordering.getId())
                                .ordering(ordering)
                                .clientId(user.getClientId())
                                .client(user.getClient())
                                .itemId(item.getId())
                                .item(item)
                                .warehouseId(warehouse.getId())
                                .warehouse(warehouse)
                                .supplierProductId(supplierProduct.getId())
                                .supplierProduct(supplierProduct)
                                .registrationDate(new Date(System.currentTimeMillis()))
                                .updateDate(new Date(System.currentTimeMillis()))
                                .tokenUser(user.getUsername())
                                .status(true)
                        .build());
            }
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
    public Page<OrderStockDTO> list(String warehouse, Long orderId, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<OrderStock> pageOrderStock;
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
            pageOrderStock = orderStockRepositoryCustom.searchForOrderStock(warehouseData,orderId,userdata,true,sort,sortColumn,pageNumber,pageSize);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageOrderStock.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<OrderStockDTO> orderStockDTOList = pageOrderStock.getContent().stream().map(orderStock -> {
            Warehouse orderStockWarehouse = warehouseRepository.findById(orderStock.getWarehouseId()).orElse(null);
            Item item = itemRepository.findByIdAndOrderId(orderStock.getItemId(),orderStock.getOrderId());
            return OrderStockDTO.builder()
                    .orderId(orderStock.getOrderId())
                    .warehouse(orderStockWarehouse.getName())
                    .itemId(orderStock.getItemId())
                    .serialSupplierProduct(orderStock.getSupplierProduct().getSerial())
                    .quantity(orderStock.getQuantity())
                    .registrationDate(orderStock.getRegistrationDate())
                    .updateDate(orderStock.getUpdateDate())
                    .build();
        }).toList();

        return new PageImpl<>(orderStockDTOList,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
    }

    @Override
    public Page<OrderStockDTO> listFalse(String warehouse, Long orderId, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<OrderStock> pageOrderStock;
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
            pageOrderStock = orderStockRepositoryCustom.searchForOrderStock(warehouseData,orderId,userdata,false,sort,sortColumn,pageNumber,pageSize);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageOrderStock.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<OrderStockDTO> orderStockDTOList = pageOrderStock.getContent().stream().map(orderStock -> {
            Warehouse orderStockWarehouse = warehouseRepository.findById(orderStock.getWarehouseId()).orElse(null);
            return OrderStockDTO.builder()
                    .orderId(orderStock.getOrderId())
                    .warehouse(orderStockWarehouse.getName())
                    .itemId(orderStock.getItemId())
                    .serialSupplierProduct(orderStock.getSupplierProduct().getSerial())
                    .quantity(orderStock.getQuantity())
                    .registrationDate(orderStock.getRegistrationDate())
                    .updateDate(orderStock.getUpdateDate())
                    .build();
        }).toList();

        return new PageImpl<>(orderStockDTOList,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
    }
}
