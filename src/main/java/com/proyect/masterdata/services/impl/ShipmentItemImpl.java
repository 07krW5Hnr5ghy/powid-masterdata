package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.services.IShipmentItem;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ShipmentItemImpl implements IShipmentItem {

    private final UserRepository userRepository;
    private final ShipmentItemRepository shipmentItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final ShipmentItemRepositoryCustom shipmentItemRepositoryCustom;
    private final ShipmentRepository shipmentRepository;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final IAudit iAudit;
    @Override
    public ShipmentItem save(Shipment shipment, String warehouse, RequestShipmentItem requestShipmentItem,
            String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        SupplierProduct supplierProduct;
        ShipmentItem shipmentItem;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProduct().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }else{
            shipmentItem = shipmentItemRepository.findByShipmentIdAndSupplierProductId(shipment.getId(),supplierProduct.getId());
        }

        if (shipmentItem != null) {
            throw new BadRequestExceptions(Constants.ErrorShipmentExists);
        }

        try {

            ShipmentItem newShipmentItem = shipmentItemRepository.save(ShipmentItem.builder()
                            .shipment(shipment)
                            .shipmentId(shipment.getId())
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .observations(requestShipmentItem.getObservations().toUpperCase())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestShipmentItem.getQuantity())
                            .tokenUser(user.getUsername())
                    .build());

            iWarehouseStock.in(shipment.getWarehouse(),supplierProduct, requestShipmentItem.getQuantity(), user);
            iGeneralStock.in(supplierProduct.getSerial(), requestShipmentItem.getQuantity(), user.getUsername());
            iAudit.save("ADD_SHIPMENT_ITEM","ADD SHIPMENT ITEM "+newShipmentItem.getSupplierProduct().getSerial()+".",user.getUsername());
            return newShipmentItem;
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ShipmentItem> saveAsync(Shipment shipment, String warehouse, RequestShipmentItem requestShipmentItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            ShipmentItem shipmentItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestShipmentItem.getSupplierProduct().toUpperCase());
                shipmentItem = shipmentItemRepository.findByShipmentIdAndSupplierProductId(shipment.getId(),supplierProduct.getId());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            if (shipmentItem != null) {
                throw new BadRequestExceptions(Constants.ErrorShipmentExists);
            }

            try {

                ShipmentItem newShipmentItem = shipmentItemRepository.save(ShipmentItem.builder()
                        .shipment(shipment)
                        .shipmentId(shipment.getId())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .observations(requestShipmentItem.getObservations().toUpperCase())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestShipmentItem.getQuantity())
                        .tokenUser(user.getUsername())
                        .build());

                iWarehouseStock.in(shipment.getWarehouse(),supplierProduct, requestShipmentItem.getQuantity(), user);
                iGeneralStock.in(supplierProduct.getSerial(), requestShipmentItem.getQuantity(), user.getUsername());
                iAudit.save("ADD_SHIPMENT_ITEM","ADD SHIPMENT ITEM "+newShipmentItem.getSupplierProduct().getSerial()+".",user.getUsername());
                return newShipmentItem;
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String serial,String supplierProductSerial, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            Shipment shipment;
            ShipmentItem shipmentItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                shipment = shipmentRepository.findBySerial(serial.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else {
                shipmentItem = shipmentItemRepository.findByShipmentIdAndSupplierProductId(shipment.getId(),supplierProduct.getId());
            }

            if (shipmentItem == null) {
                throw new BadRequestExceptions(Constants.ErrorShipment);
            }

            try {
                shipmentItem.setStatus(false);
                shipmentItem.setUpdateDate(new Date(System.currentTimeMillis()));
                shipmentItem.setTokenUser(user.getUsername());
                iWarehouseStock.out(shipment.getWarehouse(),supplierProduct, shipmentItem.getQuantity(), user);
                iGeneralStock.out(supplierProduct.getSerial(), shipmentItem.getQuantity(), user.getUsername());
                iAudit.save("DELETE_SHIPMENT_ITEM","DELETE SHIPMENT ITEM "+shipmentItem.getSupplierProduct().getSerial()+".",user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String purchaseSerial, String supplierProductSerial, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            Shipment shipment;
            ShipmentItem shipmentItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                shipment = shipmentRepository.findBySerial(purchaseSerial.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else {
                shipmentItem = shipmentItemRepository.findByShipmentIdAndSupplierProductId(shipment.getId(),supplierProduct.getId());
            }

            if (shipmentItem == null) {
                throw new BadRequestExceptions(Constants.ErrorShipment);
            }

            try {
                shipmentItem.setStatus(true);
                shipmentItem.setUpdateDate(new Date(System.currentTimeMillis()));
                shipmentItem.setTokenUser(user.getUsername());
                iWarehouseStock.in(shipment.getWarehouse(),supplierProduct, shipmentItem.getQuantity(), user);
                iGeneralStock.in(supplierProduct.getSerial(), shipmentItem.getQuantity(), user.getUsername());
                iAudit.save("ACTIVATE_SHIPMENT_ITEM","ACTIVATE SHIPMENT ITEM "+shipmentItem.getSupplierProduct().getSerial()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<ShipmentItemDTO>> list(
            String user,
            List<String> shipments,
            List<String> warehouses,
            List<String> supplierProducts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<ShipmentItem> pageShipmentItem;
            Long clientId;
            List<Long> shipmentIds;
            List<Long> purchaseIds;
            List<Long> warehouseIds;
            List<Long> supplierProductIds;

            if(shipments != null && !shipments.isEmpty()){
                shipmentIds = shipmentRepository.findBySerialIn(
                        shipments.stream().map(String::toUpperCase).toList()
                ).stream().map(Shipment::getId).toList();
            }else{
                shipmentIds = new ArrayList<>();
            }

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            if(supplierProducts != null && !supplierProducts.isEmpty()){
                supplierProductIds = supplierProductRepository.findBySerialIn(
                        supplierProducts.stream().map(String::toUpperCase).toList()
                ).stream().map(SupplierProduct::getId).toList();
            }else{
                supplierProductIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageShipmentItem = shipmentItemRepositoryCustom.searchForShipmentItem(
                        clientId,
                        shipmentIds,
                        warehouseIds,
                        supplierProductIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if (pageShipmentItem.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<ShipmentItemDTO> shipmentItemDTOS = pageShipmentItem.getContent().stream().map(shipmentItem -> ShipmentItemDTO.builder()
                    .shipment(shipmentItem.getShipment().getSerial())
                    .quantity(shipmentItem.getQuantity())
                    .supplierProduct(shipmentItem.getSupplierProduct().getSerial())
                    .warehouse(shipmentItem.getShipment().getWarehouse().getName())
                    .registrationDate(shipmentItem.getRegistrationDate())
                    .build()).toList();

            return new PageImpl<>(shipmentItemDTOS, pageShipmentItem.getPageable(), pageShipmentItem.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<ShipmentItemDTO>> listShipmentItem(String user,Long id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<ShipmentItem> shipmentItems;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    shipmentItems = shipmentItemRepository.findAllByClientIdAndShipmentId(clientId,id);
                }else{
                    shipmentItems = shipmentItemRepository.findAllByClientId(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (shipmentItems.isEmpty()){
                return Collections.emptyList();
            }

            return shipmentItems.stream().map(shipmentItem -> ShipmentItemDTO.builder()
                    .shipment(shipmentItem.getShipment().getSerial())
                    .quantity(shipmentItem.getQuantity())
                    .supplierProduct(shipmentItem.getSupplierProduct().getSerial())
                    .warehouse(shipmentItem.getShipment().getWarehouse().getName())
                    .registrationDate(shipmentItem.getRegistrationDate())
                    .build()).toList();
        });
    }

}
