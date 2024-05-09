package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.ShipmentDTO;
import com.proyect.masterdata.dto.request.RequestShipment;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class ShipmentImpl implements IShipment {

    private final UserRepository userRepository;
    private final ShipmentRepository shipmentRepository;
    private final WarehouseRepository warehouseRepository;
    private final PurchaseRepository purchaseRepository;
    private final IStockTransaction iStockTransaction;
    private final IShipmentItem iShipmentItem;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final ShipmentTypeRepository shipmentTypeRepository;
    private final ShipmentRepositoryCustom shipmentRepositoryCustom;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnRepository stockReturnRepository;

    @Override
    public ResponseSuccess save(RequestShipment requestShipment, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouse;
        Shipment shipment;
        Purchase purchase;
        ShipmentType shipmentType;
        StockReturn stockReturn;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            shipmentType = shipmentTypeRepository.findByNameAndStatusTrue(requestShipment.getShipmentType().toUpperCase());
            warehouse = warehouseRepository.findByNameAndStatusTrue(requestShipment.getWarehouse().toUpperCase());
            shipment = shipmentRepository.findByPurchaseSerialAndShipmentTypeId(requestShipment.getPurchaseSerial().toUpperCase(),shipmentType.getId());
            purchase = purchaseRepository.findBySerialAndStatusTrue(requestShipment.getPurchaseSerial().toUpperCase());
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

        try{
            if(Objects.equals(shipmentType.getName(), "DEVOLUCION")){
                stockReturn = stockReturnRepository.findBySerial(requestShipment.getPurchaseSerial());
                if(stockReturn == null){
                    throw new BadRequestExceptions(Constants.ErrorShipmentReturn);
                }
            }

            for(RequestShipmentItem requestShipmentItem : requestShipment.getRequestShipmentItemList()){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial());
                if(supplierProduct == null){
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }
            }
            List<RequestStockTransactionItem> requestStockTransactionItemList = requestShipment.getRequestShipmentItemList().stream().map(shipmentItem -> RequestStockTransactionItem.builder()
                    .quantity(shipmentItem.getQuantity())
                    .supplierProductSerial(shipmentItem.getSupplierProductSerial().toUpperCase())
                    .build()).toList();
            StockTransaction newStockTransaction = iStockTransaction.save("S"+requestShipment.getPurchaseSerial().toUpperCase(), warehouse,requestStockTransactionItemList,"ENTRADA",user);
            Shipment newShipment = shipmentRepository.save(Shipment.builder()
                            .purchaseSerial(requestShipment.getPurchaseSerial().toUpperCase())
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
            for(RequestShipmentItem requestShipmentItem : requestShipment.getRequestShipmentItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial());
                  iShipmentItem.save(newShipment,purchase,warehouse.getName(),requestShipmentItem,user.getUsername());
                  iWarehouseStock.in(warehouse,supplierProduct,requestShipmentItem.getQuantity(),user);
                  iGeneralStock.in(requestShipmentItem.getSupplierProductSerial(),requestShipmentItem.getQuantity(),user.getUsername());
            }
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(RequestShipment requestShipment, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            Shipment shipment;
            Purchase purchase;
            ShipmentType shipmentType;
            StockReturn stockReturn;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                shipmentType = shipmentTypeRepository.findByNameAndStatusTrue(requestShipment.getShipmentType().toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestShipment.getWarehouse().toUpperCase());
                shipment = shipmentRepository.findByPurchaseSerialAndShipmentTypeId(requestShipment.getPurchaseSerial().toUpperCase(),shipmentType.getId());
                purchase = purchaseRepository.findBySerialAndStatusTrue(requestShipment.getPurchaseSerial().toUpperCase());
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

            try{
                if(Objects.equals(shipmentType.getName(), "DEVOLUCION")){
                    stockReturn = stockReturnRepository.findBySerial(requestShipment.getPurchaseSerial());
                    if(stockReturn == null){
                        throw new BadRequestExceptions(Constants.ErrorShipmentReturn);
                    }
                }

                for(RequestShipmentItem requestShipmentItem : requestShipment.getRequestShipmentItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial());
                    if(supplierProduct == null){
                        throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                    }
                }
                List<RequestStockTransactionItem> requestStockTransactionItemList = requestShipment.getRequestShipmentItemList().stream().map(shipmentItem -> RequestStockTransactionItem.builder()
                        .quantity(shipmentItem.getQuantity())
                        .supplierProductSerial(shipmentItem.getSupplierProductSerial().toUpperCase())
                        .build()).toList();
                StockTransaction newStockTransaction = iStockTransaction.save("S"+requestShipment.getPurchaseSerial().toUpperCase(), warehouse,requestStockTransactionItemList,"ENTRADA",user);
                Shipment newShipment = shipmentRepository.save(Shipment.builder()
                        .purchaseSerial(requestShipment.getPurchaseSerial().toUpperCase())
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
                for(RequestShipmentItem requestShipmentItem : requestShipment.getRequestShipmentItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial());
                    iShipmentItem.save(newShipment,purchase,warehouse.getName(),requestShipmentItem,user.getUsername());
                    iWarehouseStock.in(warehouse,supplierProduct,requestShipmentItem.getQuantity(),user);
                    iGeneralStock.in(requestShipmentItem.getSupplierProductSerial(),requestShipmentItem.getQuantity(),user.getUsername());
                }
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
    public CompletableFuture<Page<ShipmentDTO>> list(String purchaseSerial, String user, String warehouse, String shipmentType, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Shipment> pageShipment;
            Long clientId;
            Long warehouseId;
            Long shipmentTypeId;

            if (warehouse != null) {
                warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
            } else {
                warehouseId = null;
            }

            if (shipmentType != null){
                shipmentTypeId = shipmentTypeRepository.findByNameAndStatusTrue(shipmentType.toUpperCase()).getId();
            }else {
                shipmentTypeId = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageShipment = shipmentRepositoryCustom.searchForShipment(clientId,purchaseSerial,warehouseId,shipmentTypeId,sort,sortColumn,pageNumber,pageSize,true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if(pageShipment.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<ShipmentDTO> shipmentDTOS = pageShipment.getContent().stream().map(shipment -> ShipmentDTO.builder()
                    .purchaseSerial(shipment.getPurchaseSerial())
                    .warehouse(shipment.getWarehouse().getName())
                    .shipmentType(shipment.getShipmentType().getName())
                    .registrationDate(shipment.getRegistrationDate())
                    .build()).toList();

            return new PageImpl<>(shipmentDTOS,pageShipment.getPageable(),pageShipment.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ShipmentDTO>> listFalse(String purchaseSerial, String user, String warehouse, String shipmentType, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Shipment> pageShipment;
            Long clientId;
            Long warehouseId;
            Long shipmentTypeId;

            if (warehouse != null) {
                warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
            } else {
                warehouseId = null;
            }

            if (shipmentType != null){
                shipmentTypeId = shipmentTypeRepository.findByNameAndStatusTrue(shipmentType.toUpperCase()).getId();
            }else {
                shipmentTypeId = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageShipment = shipmentRepositoryCustom.searchForShipment(clientId,purchaseSerial,warehouseId,shipmentTypeId,sort,sortColumn,pageNumber,pageSize,false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if(pageShipment.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<ShipmentDTO> shipmentDTOS = pageShipment.getContent().stream().map(shipment -> ShipmentDTO.builder()
                    .purchaseSerial(shipment.getPurchaseSerial())
                    .warehouse(shipment.getWarehouse().getName())
                    .shipmentType(shipment.getShipmentType().getName())
                    .registrationDate(shipment.getRegistrationDate())
                    .build()).toList();

            return new PageImpl<>(shipmentDTOS,pageShipment.getPageable(),pageShipment.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<ShipmentDTO>> listShipment(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Shipment> shipments;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                shipments = shipmentRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(shipments.isEmpty()){
                return Collections.emptyList();
            }

            return shipments.stream().map(shipment -> ShipmentDTO.builder()
                    .purchaseSerial(shipment.getPurchaseSerial())
                    .warehouse(shipment.getWarehouse().getName())
                    .shipmentType(shipment.getShipmentType().getName())
                    .registrationDate(shipment.getRegistrationDate())
                    .id(shipment.getId())
                    .build()).toList();
        });
    }

}
