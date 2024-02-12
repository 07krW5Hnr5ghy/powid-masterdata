package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderStockRepository;
import com.proyect.masterdata.repository.OrderingRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.services.IOrderStock;
import com.proyect.masterdata.services.IOrderStockItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderStockImpl implements IOrderStock {
    private final UserRepository userRepository;
    private final WarehouseRepository warehouseRepository;
    private final OrderStockRepository orderStockRepository;
    private final OrderingRepository orderingRepository;
    private final IOrderStockItem iOrderStockItem;
    @Override
    public ResponseSuccess save(Long orderId, String warehouse, List<RequestOrderStockItem> requestOrderStockItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouseData;
        Ordering ordering;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
            ordering = orderingRepository.findById(orderId).orElse(null);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(warehouseData == null){
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if(ordering == null){
            throw new BadRequestExceptions(Constants.ErrorOrdering);
        }

        try{
            for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
               Boolean existsStock = iOrderStockItem.checkWarehouseItemStock(ordering.getId(),warehouseData,requestOrderStockItem);
               if(!existsStock){
                   throw new BadRequestExceptions(Constants.ErrorOrderStockQuantity);
               }
            }
            OrderStock orderStock = orderStockRepository.save(OrderStock.builder()
                            .ordering(ordering)
                            .orderId(ordering.getId())
                            .status(true)
                            .warehouse(warehouseData)
                            .warehouseId(warehouseData.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                    .build());
            for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                iOrderStockItem.save(orderStock,requestOrderStockItem,user.getUsername());
            }
            return ResponseSuccess.builder()
                    .message(Constants.register)
                    .code(200)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
