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

import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class ShipmentImpl implements IShipment {

    private final UserRepository userRepository;
    private final ShipmentRepository shipmentRepository;
    private final WarehouseRepository warehouseRepository;
    private final IStockTransaction iStockTransaction;
    private final IShipmentItem iShipmentItem;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final ShipmentTypeRepository shipmentTypeRepository;
    private final ShipmentRepositoryCustom shipmentRepositoryCustom;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnRepository stockReturnRepository;
    private final ShipmentDocumentRepository shipmentDocumentRepository;
    private final IAudit iAudit;
    private final SupplierRepository supplierRepository;
    @Override
    public ResponseSuccess save(RequestShipment requestShipment, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouse;
        Shipment shipment;
        ShipmentType shipmentType;
        StockReturn stockReturn;
        ShipmentDocument shipmentDocument;
        Supplier supplier;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouse = warehouseRepository.findByNameAndStatusTrue(requestShipment.getWarehouse().toUpperCase());
            shipmentType = shipmentTypeRepository.findByNameAndStatusTrue(requestShipment.getShipmentType().toUpperCase());
            shipmentDocument = shipmentDocumentRepository.findByNameAndStatusTrue(requestShipment.getShipmentDocument());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestShipment.getSupplier(), user.getClientId());
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if(shipmentType == null){
            throw new BadRequestExceptions(Constants.ErrorShipmentType);
        }else{
            shipment = shipmentRepository.findBySerialAndShipmentTypeId(requestShipment.getSerial(),shipmentType.getId());
        }

        if (shipment != null) {
            throw new BadRequestExceptions(Constants.ErrorShipmentExists);
        }

        if(shipmentDocument == null){
            throw new BadRequestExceptions(Constants.ErrorShipmentDocument);
        }

        if(supplier==null){
            throw new BadRequestExceptions(Constants.ErrorSupplier);
        }

        try{
            if(Objects.equals(shipmentType.getName(), "DEVOLUCION")){
                stockReturn = stockReturnRepository.findBySerial(requestShipment.getSerial());
                if(stockReturn == null){
                    throw new BadRequestExceptions(Constants.ErrorShipmentReturn);
                }
            }

            for(RequestShipmentItem requestShipmentItem : requestShipment.getRequestShipmentItemList()){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial());
                if(supplierProduct == null){
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }
                if(requestShipmentItem.getQuantity() < 1){
                    throw new BadRequestExceptions(Constants.ErrorShipmentItemZero);
                }
            }
            List<RequestStockTransactionItem> requestStockTransactionItemList = requestShipment.getRequestShipmentItemList().stream().map(shipmentItem -> RequestStockTransactionItem.builder()
                    .quantity(shipmentItem.getQuantity())
                    .supplierProductSerial(shipmentItem.getSupplierProductSerial().toUpperCase())
                    .build()).toList();
            StockTransaction newStockTransaction = iStockTransaction.save("S"+requestShipment.getSerial().toUpperCase(), warehouse,requestStockTransactionItemList,"EMBARQUE",user);
            Shipment newShipment = shipmentRepository.save(Shipment.builder()
                            .serial(requestShipment.getSerial().toUpperCase())
                            .supplier(supplier)
                            .supplierId(supplier.getId())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .warehouse(warehouse)
                            .warehouseId(warehouse.getId())
                            .shipmentType(shipmentType)
                            .shipmentTypeId(shipmentType.getId())
                            .client(user.getClient())
                            .shipmentDocument(shipmentDocument)
                            .shipmentDocumentId(shipmentDocument.getId())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                      .build());
            for(RequestShipmentItem requestShipmentItem : requestShipment.getRequestShipmentItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial());
                  iShipmentItem.save(newShipment,warehouse.getName(),requestShipmentItem,user.getUsername());
                  iWarehouseStock.in(warehouse,supplierProduct,requestShipmentItem.getQuantity(),user);
                  iGeneralStock.in(requestShipmentItem.getSupplierProductSerial(),requestShipmentItem.getQuantity(),user.getUsername());
            }
            iAudit.save("ADD_SHIPMENT","ADD SHIPMENT " + newShipment.getSerial() +".",user.getUsername());
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
            ShipmentType shipmentType;
            StockReturn stockReturn;
            ShipmentDocument shipmentDocument;
            Supplier supplier;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestShipment.getWarehouse().toUpperCase());
                shipmentType = shipmentTypeRepository.findByNameAndStatusTrue(requestShipment.getShipmentType().toUpperCase());
                shipmentDocument = shipmentDocumentRepository.findByNameAndStatusTrue(requestShipment.getShipmentDocument());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestShipment.getSupplier(), user.getClientId());
            }

            if (warehouse == null) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if(shipmentType == null){
                throw new BadRequestExceptions(Constants.ErrorShipmentType);
            }else{
                shipment = shipmentRepository.findBySerialAndShipmentTypeId(requestShipment.getSerial(),shipmentType.getId());
            }

            if (shipment != null) {
                throw new BadRequestExceptions(Constants.ErrorShipmentExists);
            }

            if(shipmentDocument == null){
                throw new BadRequestExceptions(Constants.ErrorShipmentDocument);
            }

            if(supplier == null){
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }

            try{
                if(Objects.equals(shipmentType.getName(), "DEVOLUCION")){
                    stockReturn = stockReturnRepository.findBySerial(requestShipment.getSerial());
                    if(stockReturn == null){
                        throw new BadRequestExceptions(Constants.ErrorShipmentReturn);
                    }
                }

                for(RequestShipmentItem requestShipmentItem : requestShipment.getRequestShipmentItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial());
                    if(supplierProduct == null){
                        throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                    }
                    if(requestShipmentItem.getQuantity() < 1){
                        throw new BadRequestExceptions(Constants.ErrorShipmentItemZero);
                    }
                }
                List<RequestStockTransactionItem> requestStockTransactionItemList = requestShipment.getRequestShipmentItemList().stream().map(shipmentItem -> RequestStockTransactionItem.builder()
                        .quantity(shipmentItem.getQuantity())
                        .supplierProductSerial(shipmentItem.getSupplierProductSerial().toUpperCase())
                        .build()).toList();
                StockTransaction newStockTransaction = iStockTransaction.save("S"+requestShipment.getSerial().toUpperCase(), warehouse,requestStockTransactionItemList,"EMBARQUE",user);
                Shipment newShipment = shipmentRepository.save(Shipment.builder()
                        .serial(requestShipment.getSerial().toUpperCase())
                        .status(true)
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .shipmentType(shipmentType)
                        .shipmentTypeId(shipmentType.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                                .shipmentDocument(shipmentDocument)
                                .shipmentDocumentId(shipmentDocument.getId())
                        .tokenUser(user.getUsername())
                        .build());
                for(RequestShipmentItem requestShipmentItem : requestShipment.getRequestShipmentItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial());
                    iShipmentItem.save(newShipment,warehouse.getName(),requestShipmentItem,user.getUsername());
                    iWarehouseStock.in(warehouse,supplierProduct,requestShipmentItem.getQuantity(),user);
                    iGeneralStock.in(requestShipmentItem.getSupplierProductSerial(),requestShipmentItem.getQuantity(),user.getUsername());
                }
                iAudit.save("ADD_SHIPMENT","ADD SHIPMENT " + newShipment.getSerial() + ".",user.getUsername());
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
    public CompletableFuture<Page<ShipmentDTO>> list(
            List<String> serials,
            String user,
            List<String> warehouses,
            List<String> shipmentTypes,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Shipment> pageShipment;
            List<String> serialsUppercase;
            Long clientId;
            List<Long> warehouseIds;
            List<Long> shipmentTypeIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else{
                serialsUppercase = new ArrayList<>();
            }

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            if (shipmentTypes != null && !shipmentTypes.isEmpty()){
                shipmentTypeIds = shipmentTypeRepository.findByNameIn(
                        shipmentTypes.stream().map(String::toUpperCase).toList()
                ).stream().map(ShipmentType::getId).toList();
            }else {
                shipmentTypeIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageShipment = shipmentRepositoryCustom.searchForShipment(
                        clientId,
                        serialsUppercase,
                        warehouseIds,
                        shipmentTypeIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if(pageShipment.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<ShipmentDTO> shipmentDTOS = pageShipment.getContent().stream().map(shipment -> ShipmentDTO.builder()
                    .serial(shipment.getSerial())
                    .shipmentDocument(shipment.getShipmentDocument().getName())
                    .warehouse(shipment.getWarehouse().getName())
                    .shipmentType(shipment.getShipmentType().getName())
                    .registrationDate(shipment.getRegistrationDate())
                    .build()).toList();

            return new PageImpl<>(shipmentDTOS,pageShipment.getPageable(),pageShipment.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ShipmentDTO>> listFalse(
            List<String> serials,
            String user,
            List<String> warehouses,
            List<String> shipmentTypes,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Shipment> pageShipment;
            Long clientId;
            List<String> serialsUppercase;
            List<Long> warehouseIds;
            List<Long> shipmentTypeIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else{
                serialsUppercase = new ArrayList<>();
            }

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            if (shipmentTypes != null && !shipmentTypes.isEmpty()){
                shipmentTypeIds = shipmentTypeRepository.findByNameIn(
                        shipmentTypes.stream().map(String::toUpperCase).toList()
                ).stream().map(ShipmentType::getId).toList();
            }else {
                shipmentTypeIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageShipment = shipmentRepositoryCustom.searchForShipment(
                        clientId,
                        serialsUppercase,
                        warehouseIds,
                        shipmentTypeIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if(pageShipment.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<ShipmentDTO> shipmentDTOS = pageShipment.getContent().stream().map(shipment -> ShipmentDTO.builder()
                    .serial(shipment.getSerial())
                    .shipmentDocument(shipment.getShipmentDocument().getName())
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
                    .serial(shipment.getSerial())
                    .shipmentDocument(shipment.getShipmentDocument().getName())
                    .warehouse(shipment.getWarehouse().getName())
                    .shipmentType(shipment.getShipmentType().getName())
                    .registrationDate(shipment.getRegistrationDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<ShipmentDTO>> listFilter(String user) throws BadRequestExceptions, InternalErrorExceptions {
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
                    .serial(shipment.getSerial())
                    .shipmentDocument(shipment.getShipmentDocument().getName())
                    .warehouse(shipment.getWarehouse().getName())
                    .shipmentType(shipment.getShipmentType().getName())
                    .registrationDate(shipment.getRegistrationDate())
                    .build()).toList();
        });
    }

}
