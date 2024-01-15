package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.Shipment;
import com.proyect.masterdata.domain.StockTransaction;
import com.proyect.masterdata.domain.StockTransactionType;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.ShipmentDTO;
import com.proyect.masterdata.dto.request.RequestShipment;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseRepository;
import com.proyect.masterdata.repository.ShipmentRepository;
import com.proyect.masterdata.repository.ShipmentRepositoryCustom;
import com.proyect.masterdata.repository.StockTransactionRepository;
import com.proyect.masterdata.repository.StockTransactionTypeRepository;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.services.IShipment;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ShipmentImpl implements IShipment {

    private final UserRepository userRepository;
    private final ShipmentRepository shipmentRepository;
    private final WarehouseRepository warehouseRepository;
    private final PurchaseRepository purchaseRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final StockTransactionTypeRepository stockTransactionTypeRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final ShipmentRepositoryCustom shipmentRepositoryCustom;
    private final IWarehouseStock iWarehouseStock;

    @Override
    public ResponseSuccess save(String serial, String warehouse, List<RequestShipment> requestShipmentList,
            String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Warehouse warehouseData;
        Shipment shipment;
        StockTransactionType stockTransactionType;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
            shipment = shipmentRepository.findBySerial(serial.toUpperCase());
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

        if (warehouseData.getClientId() != user.getClientId()) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (shipment != null) {
            throw new BadRequestExceptions(Constants.ErrorShipmentExists);
        }

        try {

            for (RequestShipment requestShipment : requestShipmentList) {

                SupplierProduct supplierProduct = supplierProductRepository
                        .findBySerialAndStatusTrue(requestShipment.getSupplierProductSerial());

                if (supplierProduct == null) {
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }

                iWarehouseStock.in(warehouse, requestShipment.getSupplierProductSerial(), requestShipment.getQuantity(),
                        tokenUser);

                StockTransaction existentStockTransaction = stockTransactionRepository.findBySerialAndSupplierProductId(
                        serial,
                        supplierProduct.getId());

                if (existentStockTransaction != null) {
                    throw new BadRequestExceptions(Constants.ErrorStockTransactionExists);
                }

                Purchase purchase = purchaseRepository
                        .findBySerialAndSupplierProductId(requestShipment.getPurchaseSerial(), supplierProduct.getId());

                if (purchase == null) {
                    throw new BadRequestExceptions(Constants.ErrorPurchase);
                }

                StockTransaction stockTransaction = stockTransactionRepository.save(StockTransaction.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestShipment.getQuantity())
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

                shipmentRepository.save(Shipment.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .observations(requestShipment.getObservations())
                        .purchase(purchase)
                        .purchaseId(purchase.getId())
                        .quantity(requestShipment.getQuantity())
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
    public Page<ShipmentDTO> list(String serial, String user, String warehouse, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        Page<Shipment> pageShipment;
        Long clientId;
        Long warehouseId;

        if (warehouse != null) {
            warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
        } else {
            warehouseId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageShipment = shipmentRepositoryCustom.searchForShipment(clientId, serial, warehouseId, sort, sortColumn,
                    pageNumber, pageSize);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.ResultsFound);
        }

        if (pageShipment.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<ShipmentDTO> shipmentDTOs = pageShipment.getContent().stream().map(shipment -> ShipmentDTO.builder()
                .purchaseSerial(shipment.getPurchase().getSerial())
                .quantity(shipment.getQuantity())
                .serial(shipment.getSerial())
                .supplierProductSerial(shipment.getSupplierProduct().getSerial())
                .warehouse(shipment.getWarehouse().getName())
                .date(shipment.getRegistrationDate())
                .build()).toList();

        return new PageImpl<>(shipmentDTOs, pageShipment.getPageable(), pageShipment.getTotalElements());
    }

}
