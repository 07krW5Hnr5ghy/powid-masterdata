package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.ClientDTO;
import com.proyect.masterdata.dto.request.RequestClient;
import com.proyect.masterdata.dto.request.RequestClientSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ClientMapper;
import com.proyect.masterdata.repository.ClientRepository;
import com.proyect.masterdata.repository.ClientRepositoryCustom;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IClient;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class ClientImpl implements IClient {

    private final UserRepository userRepository;
    private final ClientRepository clientRepository;
    private final DistrictRepository districtRepository;
    private final ClientMapper clientMapper;
    private final ClientRepositoryCustom clientRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(RequestClientSave requestClientSave)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsClient;
        District district;

        try {
            existsClient = clientRepository.existsByRuc(requestClientSave.getRuc());
            district = districtRepository.findByNameAndStatusTrue(requestClientSave.getDistrict().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (existsClient) {
            throw new BadRequestExceptions(Constants.ErrorClientExist);
        }

        if (district == null) {
            throw new BadRequestExceptions(Constants.ErrorDistrict);
        }

        try {

            clientRepository.save(Client.builder()
                    .name(requestClientSave.getName().toUpperCase())
                    .surname(requestClientSave.getSurname().toUpperCase())
                    .business(requestClientSave.getBusiness().toUpperCase())
                    .dni(requestClientSave.getDni())
                    .email(requestClientSave.getEmail())
                    .ruc(requestClientSave.getRuc())
                    .address(requestClientSave.getAddress().toUpperCase())
                    .mobile(requestClientSave.getMobile())
                    .district(district)
                    .districtId(district.getId())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestClientSave requestClientSave) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            boolean existsClient;
            District district;

            try {
                existsClient = clientRepository.existsByRuc(requestClientSave.getRuc());
                district = districtRepository.findByNameAndStatusTrue(requestClientSave.getDistrict().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (existsClient) {
                throw new BadRequestExceptions(Constants.ErrorClientExist);
            }

            if (district == null) {
                throw new BadRequestExceptions(Constants.ErrorDistrict);
            }

            try {

                clientRepository.save(Client.builder()
                        .name(requestClientSave.getName().toUpperCase())
                        .surname(requestClientSave.getSurname().toUpperCase())
                        .business(requestClientSave.getBusiness().toUpperCase())
                        .dni(requestClientSave.getDni())
                        .email(requestClientSave.getEmail())
                        .ruc(requestClientSave.getRuc())
                        .address(requestClientSave.getAddress().toUpperCase())
                        .mobile(requestClientSave.getMobile())
                        .ruc(requestClientSave.getRuc())
                        .district(district)
                        .districtId(district.getId())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .build());
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
    public CompletableFuture<ClientDTO> update(RequestClient requestClient, String username)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Client client;
            District district;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                client = clientRepository.findByRucAndStatusTrue(requestClient.getRuc());
                district = districtRepository.findByNameAndStatusTrue(requestClient.getDistrict());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (client == null) {
                throw new BadRequestExceptions(Constants.ErrorClient);
            }

            try {

                client.setName(requestClient.getName().toUpperCase());
                client.setSurname(requestClient.getSurname().toUpperCase());
                client.setDni(requestClient.getDni());
                client.setUpdateDate(new Date(System.currentTimeMillis()));
                client.setMobile(requestClient.getMobile());
                client.setAddress(requestClient.getAddress().toUpperCase());
                client.setEmail(requestClient.getEmail());
                client.setDistrict(district);
                client.setDistrictId(district.getId());
                clientRepository.save(client);
                ClientDTO clientDTO = clientMapper.clientToClientDTO(client);
                clientDTO.setDistrict(district.getName());
                iAudit.save("UPDATE_CLIENT","UPDATE CLIENT "+client.getRuc()+".",user.getUsername());
                return clientDTO;
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });

    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String ruc, String username) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Client client;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                client = clientRepository.findByRucAndStatusTrue(ruc);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if (client == null) {
                throw new BadRequestExceptions(Constants.ErrorClient);
            }
            try {
                client.setStatus(false);
                client.setUpdateDate(new Date(System.currentTimeMillis()));
                clientRepository.save(client);
                iAudit.save("DELETE_CLIENT","DELETE CLIENT "+client.getRuc()+".",user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<ClientDTO>> list(String ruc, String business, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Client> clientPage;

            try {
                clientPage = clientRepositoryCustom.searchForClient(
                        ruc,
                        business,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize, true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (clientPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<ClientDTO> clientDTOList = clientPage.getContent().stream().map(client -> ClientDTO.builder()
                    .name(client.getName().toUpperCase())
                    .surname(client.getSurname().toUpperCase())
                    .business(client.getBusiness().toUpperCase())
                    .dni(client.getDni())
                    .email(client.getEmail())
                    .ruc(client.getRuc())
                    .address(client.getAddress().toUpperCase())
                    .mobile(client.getMobile())
                    .ruc(client.getRuc())
                    .district(client.getDistrict().getName())
                    .status(client.getStatus())
                    .build()).toList();

            return new PageImpl<>(clientDTOList,
                    clientPage.getPageable(), clientPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ClientDTO>> listFalse(String ruc, String business, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Client> clientPage;

            try {
                clientPage = clientRepositoryCustom.searchForClient(ruc, business,registrationStartDate,registrationEndDate,updateStartDate,updateStartDate, sort, sortColumn, pageNumber,
                        pageSize, false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (clientPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<ClientDTO> clientDTOList = clientPage.getContent().stream().map(client -> ClientDTO.builder()
                    .name(client.getName().toUpperCase())
                    .surname(client.getSurname().toUpperCase())
                    .business(client.getBusiness().toUpperCase())
                    .dni(client.getDni())
                    .email(client.getEmail())
                    .ruc(client.getRuc())
                    .address(client.getAddress().toUpperCase())
                    .mobile(client.getMobile())
                    .ruc(client.getRuc())
                    .district(client.getDistrict().getName())
                    .status(client.getStatus())
                    .build()).toList();

            return new PageImpl<>(clientDTOList,
                    clientPage.getPageable(), clientPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String ruc, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Client client;
            try {
               user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
               client = clientRepository.findByRucAndStatusFalse(ruc.toUpperCase());
            }catch (RuntimeException e){
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(client==null){
                throw new BadRequestExceptions(Constants.ErrorClient);
            }
            try {
                client.setStatus(true);
                client.setRegistrationDate(new Date(System.currentTimeMillis()));
                clientRepository.save(client);
                iAudit.save("ACTIVATE_CLIENT","ACTIVATE CLIENT "+client.getRuc()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
