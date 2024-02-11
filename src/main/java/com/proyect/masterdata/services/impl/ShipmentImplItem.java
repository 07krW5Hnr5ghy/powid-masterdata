package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import com.proyect.masterdata.domain.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.PurchaseItem;
import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseItemRepository;
import com.proyect.masterdata.repository.ShipmentItemRepository;
import com.proyect.masterdata.repository.ShipmentItemRepositoryCustom;
import com.proyect.masterdata.repository.StockTransactionRepository;
import com.proyect.masterdata.repository.StockTransactionTypeRepository;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.services.IShipmentItem;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ShipmentImplItem implements IShipmentItem {

    private final UserRepository userRepository;
    private final ShipmentItemRepository shipmentItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final StockTransactionTypeRepository stockTransactionTypeRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final ShipmentItemRepositoryCustom shipmentItemRepositoryCustom;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;

    @Override
    public ResponseSuccess save(String serial, String warehouse, List<RequestShipmentItem> requestShipmentItemList,
            String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Warehouse warehouseData;
        ShipmentItem shipmentItem;
        StockTransactionType stockTransactionType;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
            shipmentItem = shipmentItemRepository.findBySerial(serial.toUpperCase());
            stockTransactionType = stockTransactionTypeRepository.findByNameAndStatusTrue("ENTRADA");
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (warehouseData == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouseData.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (shipmentItem != null) {
            throw new BadRequestExceptions(Constants.ErrorShipmentExists);
        }

        try {

            for (RequestShipmentItem requestShipmentItem : requestShipmentItemList) {

                SupplierProduct supplierProduct = supplierProductRepository
                        .findBySerialAndStatusTrue(requestShipmentItem.getSupplierProductSerial());

                if (supplierProduct == null) {
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }

                StockTransaction existentStockTransaction = stockTransactionRepository.findBySerialAndSupplierProductId(
                        serial,
                        supplierProduct.getId());

                if (existentStockTransaction != null) {
                    throw new BadRequestExceptions(Constants.ErrorStockTransactionExists);
                }

                PurchaseItem purchaseItem = purchaseItemRepository
                        .findByPurchaseIdAndSupplierProductId(requestShipmentItem.getPurchaseSerial(), supplierProduct.getId());

                if (purchaseItem == null) {
                    throw new BadRequestExceptions(Constants.ErrorPurchase);
                }

                StockTransaction stockTransaction = stockTransactionRepository.save(StockTransaction.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestShipmentItem.getQuantity())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .serial(serial.toUpperCase())
                        .stockTransactionType(stockTransactionType)
                        .stockTransactionTypeId(stockTransactionType.getId())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .tokenUser(user.getUsername())
                        .warehouse(warehouseData)
                        .warehouseId(warehouseData.getId())
                        .build());

                shipmentItemRepository.save(ShipmentItem.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .observations(requestShipmentItem.getObservations())
                        .purchaseItem(purchaseItem)
                        .purchaseId(purchaseItem.getId())
                        .quantity(requestShipmentItem.getQuantity())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .serial(serial.toUpperCase())
                        .status(true)
                        .stockTransaction(stockTransaction)
                        .stockTransactionId(stockTransaction.getId())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .tokenUser(user.getUsername())
                        .warehouse(warehouseData)
                        .warehouseId(warehouseData.getId())
                        .build());

                iWarehouseStock.in(warehouse, requestShipmentItem.getSupplierProductSerial(), requestShipmentItem.getQuantity(),
                        tokenUser);

                iGeneralStock.in(requestShipmentItem.getSupplierProductSerial(), requestShipmentItem.getQuantity(), tokenUser);

            }

            return ResponseSuccess.builder()
                    .message(Constants.register)
                    .code(200)
                    .build();

        } catch (RuntimeException e) {
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<ShipmentItemDTO> list(String serial, String user, String warehouse, String sort, String sortColumn,
                                      Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        Page<ShipmentItem> pageShipment;
        Long clientId;
        Long warehouseId;

        if (warehouse != null) {
            warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
        } else {
            warehouseId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageShipment = shipmentItemRepositoryCustom.searchForShipment(clientId, serial, warehouseId, sort, sortColumn,
                    pageNumber, pageSize);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.ResultsFound);
        }

        if (pageShipment.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<ShipmentItemDTO> shipmentItemDTOS = pageShipment.getContent().stream().map(shipmentItem -> ShipmentItemDTO.builder()
                .purchaseSerial(shipmentItem.getPurchaseItem().getSerial())
                .quantity(shipmentItem.getQuantity())
                .serial(shipmentItem.getSerial())
                .supplierProductSerial(shipmentItem.getSupplierProduct().getSerial())
                .warehouse(shipmentItem.getWarehouse().getName())
                .date(shipmentItem.getRegistrationDate())
                .build()).toList();

        return new PageImpl<>(shipmentItemDTOS, pageShipment.getPageable(), pageShipment.getTotalElements());
    }

}
