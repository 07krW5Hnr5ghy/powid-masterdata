package com.proyect.masterdata.services.impl;

import java.util.Date;
import java.util.List;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Supplier;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.SupplierDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.SupplierRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISupplier;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class SupplierImpl implements ISupplier {

    private final UserRepository userRepository;
    private final SupplierRepository supplierRepository;

    @Override
    public ResponseSuccess save(SupplierDTO supplierData, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Supplier supplier;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplier = supplierRepository.findByRucAndStatusTrue(supplierData.getRuc());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (supplier != null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierExists);
        }

        try {
            supplierRepository.save(Supplier.builder()
                    .businessName(supplierData.getBusinessName())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .country(supplierData.getCountry())
                    .email(supplierData.getEmail())
                    .location(supplierData.getLocation())
                    .phoneNumber(supplierData.getPhoneNumber())
                    .ruc(supplierData.getRuc())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(user.getName())
                    .build());

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
    public ResponseSuccess saveAll(List<SupplierDTO> supplierDataList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;

        List<Supplier> suppliers;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            suppliers = supplierRepository
                    .findByRucInAndStatusTrue(supplierDataList.stream().map(supplier -> supplier.getRuc()).toList());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (!suppliers.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorSupplierExists);
        }

        try {
            supplierRepository.saveAll(supplierDataList.stream().map(supplier -> Supplier.builder()
                    .businessName(supplier.getBusinessName())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .country(supplier.getCountry())
                    .location(supplier.getLocation())
                    .email(supplier.getEmail())
                    .phoneNumber(supplier.getPhoneNumber())
                    .ruc(supplier.getRuc())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(tokenUser.toUpperCase())
                    .build()).toList());

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
    public ResponseDelete delete(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Supplier supplier;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplier = supplierRepository.findByRucAndStatusTrue(ruc);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }

        if (supplier == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplier);
        }

        try {
            supplier.setStatus(false);
            supplier.setUpdateDate(new Date(System.currentTimeMillis()));
            supplierRepository.save(supplier);

            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
