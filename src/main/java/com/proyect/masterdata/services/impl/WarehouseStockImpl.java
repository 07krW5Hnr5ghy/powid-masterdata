package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.domain.WarehouseStock;
import com.proyect.masterdata.dto.WarehouseDTO;
import com.proyect.masterdata.dto.WarehouseStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.repository.WarehouseStockRepository;
import com.proyect.masterdata.repository.WarehouseStockRepositoryCustom;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class WarehouseStockImpl implements IWarehouseStock {

    private final WarehouseStockRepository warehouseStockRepository;
    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final WarehouseRepository warehouseRepository;
    private final WarehouseStockRepositoryCustom warehouseStockRepositoryCustom;

    @Override
    public ResponseSuccess in(Warehouse warehouse, SupplierProduct supplierProduct, Integer quantity, User user)
            throws InternalErrorExceptions, BadRequestExceptions {

        WarehouseStock warehouseStock;

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (supplierProduct == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        try {
            warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),
                    supplierProduct.getId());

            if (warehouseStock != null) {
                warehouseStock.setQuantity(warehouseStock.getQuantity() + quantity);
                warehouseStock.setUpdateDate(new Date(System.currentTimeMillis()));
                warehouseStockRepository.save(warehouseStock);
            } else {
                warehouseStockRepository.save(WarehouseStock.builder()
                        .quantity(quantity)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .build());
            }

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess out(Warehouse warehouse, SupplierProduct supplierProduct, Integer quantity, User user)
            throws InternalErrorExceptions, BadRequestExceptions {

        WarehouseStock warehouseStock;

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (supplierProduct == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        try {
            warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),
                    supplierProduct.getId());

            if (warehouseStock == null) {
                throw new BadRequestExceptions(Constants.ErrorWarehouseStock);
            }

            if (quantity > warehouseStock.getQuantity()) {
                throw new BadRequestExceptions(Constants.ErrorWarehouseStockLess);
            }

            warehouseStock.setQuantity(warehouseStock.getQuantity() - quantity);
            warehouseStockRepository.save(warehouseStock);

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();

        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<WarehouseStockDTO> list(String warehouse, String user, String sort, String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions {
        Page<WarehouseStock> warehouseStockPage;
        Long clientId;
        Long warehouseId;

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
            warehouseStockPage = warehouseStockRepositoryCustom.searchForWarehouseStock(clientId, warehouseId, sort,
                    sortColumn,
                    pageNumber,
                    pageSize);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (warehouseStockPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<WarehouseStockDTO> warehouseStockDTOs = warehouseStockPage.getContent().stream()
                .map(warehouseStock -> WarehouseStockDTO.builder()
                        .quantity(warehouseStock.getQuantity())
                        .supplierProductSerial(warehouseStock.getSupplierProduct().getSerial())
                        .warehouse(warehouseStock.getWarehouse().getName())
                        .registrationDate(warehouseStock.getRegistrationDate())
                        .updateDate(warehouseStock.getUpdateDate())
                        .build())
                .toList();

        return new PageImpl<>(warehouseStockDTOs, warehouseStockPage.getPageable(),
                warehouseStockPage.getTotalElements());
    }

    @Override
    public List<WarehouseStockDTO> listWarehouse(String user,String warehouseName) throws BadRequestExceptions, InternalErrorExceptions {
        List<WarehouseStock> warehouseStocks;
        Warehouse warehouse;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user).getClientId();
            if(warehouseName != null){
                warehouse = warehouseRepository.findByNameAndStatusTrue(warehouseName.toUpperCase());
                warehouseStocks = warehouseStockRepository.findAllByClientIdAndWarehouseId(clientId, warehouse.getId());
            }else{
                warehouseStocks = warehouseStockRepository.findAllByClientId(clientId);
            }
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(warehouseStocks.isEmpty()){
            return Collections.emptyList();
        }

        return warehouseStocks.stream()
                .map(warehouseStock -> WarehouseStockDTO.builder()
                        .quantity(warehouseStock.getQuantity())
                        .supplierProductSerial(warehouseStock.getSupplierProduct().getSerial())
                        .warehouse(warehouseStock.getWarehouse().getName())
                        .registrationDate(warehouseStock.getRegistrationDate())
                        .updateDate(warehouseStock.getUpdateDate())
                        .build())
                .toList();
    }

}
