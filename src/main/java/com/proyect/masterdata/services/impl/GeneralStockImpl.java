package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.services.IAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.GeneralStock;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.GeneralStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.GeneralStockRepository;
import com.proyect.masterdata.repository.GeneralStockRepositoryCustom;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class GeneralStockImpl implements IGeneralStock {

    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final GeneralStockRepository generalStockRepository;
    private final GeneralStockRepositoryCustom generalStockRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> in(String supplierProductSerial, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (supplierProduct == null) {
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            try {
                GeneralStock generalStock = generalStockRepository.findByClientIdAndSupplierProductId(user.getClientId(),
                        supplierProduct.getId());
                if (generalStock != null) {
                    generalStock.setQuantity(generalStock.getQuantity() + quantity);
                    generalStock.setUpdateDate(new Date(System.currentTimeMillis()));
                    generalStock.setTokenUser(user.getUsername());
                    generalStockRepository.save(generalStock);
                } else {
                    generalStockRepository.save(GeneralStock.builder()
                            .quantity(quantity)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .tokenUser(user.getUsername())
                            .build());
                }
                iAudit.save("ADD_GENERAL_STOCK","ADD GENERAL STOCK FOR SUPPLIER PRODUCT "+supplierProduct.getSerial()+" WITH " + quantity +" UNITS.",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> out(String supplierProductSerial, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (supplierProduct == null) {
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            try {
                GeneralStock generalStock = generalStockRepository.findByClientIdAndSupplierProductId(user.getClientId(),
                        supplierProduct.getId());

                if (generalStock == null) {
                    throw new BadRequestExceptions(Constants.ErrorGeneralStock);
                }

                if (quantity > generalStock.getQuantity()) {
                    throw new BadRequestExceptions(Constants.ErrorGeneralStockLess);
                }

                generalStock.setQuantity(generalStock.getQuantity() - quantity);
                generalStockRepository.save(generalStock);
                iAudit.save("DELETE_GENERAL_STOCK","DELETE GENERAL STOCK FOR SUPPLIER PRODUCT "+supplierProduct.getSerial()+" WITH " + quantity +" UNITS.",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<GeneralStockDTO>> list(
            String user,
            String supplierProductSerial,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize)
            throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<GeneralStock> generalStockPage;
            Long clientId;
            Long supplierProductId;
            if(supplierProductSerial==null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else{
                supplierProductId = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase()).getId();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                generalStockPage = generalStockRepositoryCustom.searchForGeneralStock(
                        clientId,
                        supplierProductId,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (generalStockPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<GeneralStockDTO> generalStockDTOs = generalStockPage.getContent().stream()
                    .map(generalStock -> GeneralStockDTO.builder()
                            .quantity(generalStock.getQuantity())
                            .supplierProductSerial(generalStock.getSupplierProduct().getSerial())
                            .registrationDate(generalStock.getRegistrationDate())
                            .updateDate(generalStock.getUpdateDate())
                            .build())
                    .toList();

            return new PageImpl<>(generalStockDTOs, generalStockPage.getPageable(),
                    generalStockPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<GeneralStockDTO>> listGeneralStock(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            List<GeneralStock> generalStocks;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                generalStocks = generalStockRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(generalStocks.isEmpty()){
                return Collections.emptyList();
            }

            return generalStocks.stream()
                    .map(generalStock -> GeneralStockDTO.builder()
                            .quantity(generalStock.getQuantity())
                            .supplierProductSerial(generalStock.getSupplierProduct().getSerial())
                            .registrationDate(generalStock.getRegistrationDate())
                            .updateDate(generalStock.getUpdateDate())
                            .build())
                    .toList();
        });
    }

}
