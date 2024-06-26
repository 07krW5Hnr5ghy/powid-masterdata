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
    private final PurchaseRepository purchaseRepository;
    private final IStockTransaction iStockTransaction;
    private final IShipmentItem iShipmentItem;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final ShipmentTypeRepository shipmentTypeRepository;
    private final ShipmentRepositoryCustom shipmentRepositoryCustom;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnRepository stockReturnRepository;
    private final IAudit iAudit;
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
            warehouse = warehouseRepository.findByNameAndStatusTrue(requestShipment.getWarehouse().toUpperCase());
            purchase = purchaseRepository.findBySerialAndStatusTrue(requestShipment.getPurchaseSerial().toUpperCase());
            shipment = shipmentRepository.findBySerial(requestShipment.getSerial());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (shipment != null) {
            throw new BadRequestExceptions(Constants.ErrorShipmentExists);
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

        if(purchase == null){
            throw new BadRequestExceptions(Constants.ErrorPurchase);
        }else {
            shipmentType = shipmentTypeRepository.findByNameAndStatusTrue(requestShipment.getShipmentType().toUpperCase());
        }

        if(shipmentType == null){
            throw new BadRequestExceptions(Constants.ErrorShipmentType);
        }else{
            shipment = shipmentRepository.findByPurchaseSerialAndShipmentTypeId(requestShipment.getPurchaseSerial().toUpperCase(),shipmentType.getId());
        }

        if (shipment != null) {
            throw new BadRequestExceptions(Constants.ErrorShipmentExists);
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
            iAudit.save("ADD_SHIPMENT","ADD SHIPMENT " + newShipment.getSerial() + " FOR PURCHASE "+newShipment.getPurchase().getSerial()+".",user.getUsername());
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
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestShipment.getWarehouse().toUpperCase());
                purchase = purchaseRepository.findBySerialAndStatusTrue(requestShipment.getPurchaseSerial().toUpperCase());
                shipment = shipmentRepository.findBySerial(requestShipment.getSerial());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (shipment != null) {
                throw new BadRequestExceptions(Constants.ErrorShipmentExists);
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

            if(purchase == null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }else {
                shipmentType = shipmentTypeRepository.findByNameAndStatusTrue(requestShipment.getShipmentType().toUpperCase());
            }

            if(shipmentType == null){
                throw new BadRequestExceptions(Constants.ErrorShipmentType);
            }else{
                shipment = shipmentRepository.findByPurchaseSerialAndShipmentTypeId(requestShipment.getPurchaseSerial().toUpperCase(),shipmentType.getId());
            }

            if (shipment != null) {
                throw new BadRequestExceptions(Constants.ErrorShipmentExists);
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
                iAudit.save("ADD_SHIPMENT","ADD SHIPMENT " + newShipment.getSerial() + " FOR PURCHASE "+newShipment.getPurchase().getSerial()+".",user.getUsername());
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
            List<String> purchases,
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
            List<Long> purchaseIds;
            List<Long> warehouseIds;
            List<Long> shipmentTypeIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else{
                serialsUppercase = new ArrayList<>();
            }

            if(purchases != null && !purchases.isEmpty()){
                purchaseIds = purchaseRepository.findBySerialIn(purchases.stream().map(
                        String::toUpperCase
                ).toList()).stream().map(
                        Purchase::getId
                ).toList();
            }else{
                purchaseIds = new ArrayList<>();
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
                        purchaseIds,
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
                    .purchase(shipment.getPurchase().getSerial())
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
            List<String> purchases,
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
            List<Long> purchaseIds;
            List<Long> warehouseIds;
            List<Long> shipmentTypeIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else{
                serialsUppercase = new ArrayList<>();
            }

            if(purchases != null && !purchases.isEmpty()){
                purchaseIds = purchaseRepository.findBySerialIn(purchases.stream().map(
                        String::toUpperCase
                ).toList()).stream().map(
                        Purchase::getId
                ).toList();
            }else{
                purchaseIds = new ArrayList<>();
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
                        purchaseIds,
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
                    .purchase(shipment.getPurchase().getSerial())
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
                    .purchase(shipment.getPurchase().getSerial())
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
                    .purchase(shipment.getPurchase().getSerial())
                    .warehouse(shipment.getWarehouse().getName())
                    .shipmentType(shipment.getShipmentType().getName())
                    .registrationDate(shipment.getRegistrationDate())
                    .build()).toList();
        });
    }

}
