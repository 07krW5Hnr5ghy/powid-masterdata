package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Supplier;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.SupplierDTO;
import com.proyect.masterdata.dto.request.RequestSupplier;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.SupplierRepository;
import com.proyect.masterdata.repository.SupplierRepositoryCustom;
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
    private final SupplierRepositoryCustom supplierRepositoryCustom;

    @Override
    public ResponseSuccess save(RequestSupplier requestSupplier, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Supplier supplierRuc;
        Supplier supplierName;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            supplierRuc = supplierRepository.findByRucAndClientId(requestSupplier.getRuc(), user.getClientId());
            supplierName = supplierRepository.findByBusinessNameAndClientId(requestSupplier.getBusinessName().toUpperCase(), user.getClientId());
        }

        if (supplierRuc != null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierRucExists);
        }

        if(supplierName != null){
            throw new BadRequestExceptions(Constants.ErrorSupplierNameExists);
        }

        try {
            supplierRepository.save(Supplier.builder()
                    .businessName(requestSupplier.getBusinessName().toUpperCase())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .country(requestSupplier.getCountry().toUpperCase())
                    .email(requestSupplier.getEmail())
                    .location(requestSupplier.getLocation().toUpperCase())
                    .phoneNumber(requestSupplier.getPhoneNumber())
                    .ruc(requestSupplier.getRuc())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(user.getUsername().toUpperCase())
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
    public ResponseDelete delete(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Supplier supplier;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }else {
            supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(ruc, user.getClientId());
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

    @Override
    public Page<SupplierDTO> list(String name, String ruc, String user, String sort, String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {

        Page<Supplier> supplierPage;
        Long clientId;

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            supplierPage = supplierRepositoryCustom.searchForSupplier(name, ruc, clientId, sort, sortColumn, pageNumber,
                    pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (supplierPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<SupplierDTO> supplierDTOs = supplierPage.getContent().stream().map(supplier -> SupplierDTO.builder()
                .businessName(supplier.getBusinessName())
                .country(supplier.getCountry())
                .email(supplier.getEmail())
                .location(supplier.getLocation())
                .phoneNumber(supplier.getPhoneNumber())
                .ruc(supplier.getRuc())
                .build()).toList();

        return new PageImpl<>(supplierDTOs, supplierPage.getPageable(), supplierPage.getTotalElements());

    }

}
