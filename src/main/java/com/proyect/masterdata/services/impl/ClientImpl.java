package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.District;
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

@Service
@RequiredArgsConstructor
@Log4j2
public class ClientImpl implements IClient {
    private final UserRepository userRepository;
    private final ClientRepository clientRepository;
    private final DistrictRepository districtRepository;
    private final ClientMapper clientMapper;
    private final ClientRepositoryCustom clientRepositoryCustom;
    @Override
    public ResponseSuccess save(RequestClientSave requestClientSave, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        boolean existsClient;
        boolean existDistrict;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            existsClient = clientRepository.existsByRuc(requestClientSave.getRuc());
            existDistrict = districtRepository.existsByName(requestClientSave.getName());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(existsClient){
            throw new BadRequestExceptions("Cliente ya existe");
        }
        if(!existDistrict){
            throw new BadRequestExceptions("Distrito no existe");
        }
        try{
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
                    .id_district(districtRepository.findByNameAndStatusTrue(requestClientSave.getDistrict()).getId())
                    .status(1L)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .user(user.toUpperCase())
                    .build()
            );
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<RequestClientSave> requestClientSaveList, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        List<Client> clientList;
        List<District> districtList;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            clientList = clientRepository.findByRucIn(requestClientSaveList.stream().map(client -> client.getRuc()).toList());
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(!clientList.isEmpty()){
            throw new BadRequestExceptions("Cliente existente");
        }
        try {
            clientRepository.saveAll(requestClientSaveList.stream().map(client ->
                Client.builder()
                        .name(client.getName().toUpperCase())
                        .surname(client.getSurname().toUpperCase())
                        .business(client.getBusiness().toUpperCase())
                        .dni(client.getDni())
                        .email(client.getEmail())
                        .ruc(client.getRuc())
                        .address(client.getAddress().toUpperCase())
                        .mobile(client.getMobile())
                        .ruc(client.getRuc())
                        .id_district(districtRepository.findByNameAndStatusTrue(client.getDistrict()).getId())
                        .status(1L)
                        .dateRegistration(new Date(System.currentTimeMillis()))
                        .user(user.toUpperCase())
                        .build()
            ).toList());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ClientDTO update(RequestClient requestClient) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Client client;
        try{
            existsUser = userRepository.existsById(requestClient.getUser().toUpperCase());
            client = clientRepository.findByRuc(requestClient.getRuc());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(client==null){
            throw new BadRequestExceptions("Cliente no existe");
        }
        try {
            client.setName(requestClient.getName().toUpperCase());
            client.setSurname(requestClient.getSurname().toUpperCase());
            client.setDni(requestClient.getDni());
            client.setDateRegistration(new Date(System.currentTimeMillis()));
            client.setStatus(requestClient.getStatus());
            client.setMobile(requestClient.getMobile());
            client.setAddress(requestClient.getAddress().toUpperCase());
            client.setEmail(requestClient.getEmail());
            ClientDTO clientDTO = clientMapper.clientToClientDTO(client);
            clientDTO.setDistrict(districtRepository.findById(client.getId_district()).orElse(null).getName());
            return clientDTO;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseDelete delete(String ruc,String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Client client;
        try {
            existsUser = userRepository.existsById(user.toUpperCase());
            client = clientRepository.findByRuc(ruc);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(client==null){
            throw new BadRequestExceptions("Cliente no existe");
        }
        try {
            client.setStatus(0L);
            clientRepository.save(client);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<ClientDTO> list(String ruc, String business, String user,Long status, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        Page<Client> clientPage;
        try{
            clientPage = clientRepositoryCustom.searchForClient(ruc,business,user,sort,sortColumn,pageNumber,pageSize,status);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(clientPage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(clientMapper.listClientToListClientDTO(clientPage.getContent()),
                clientPage.getPageable(),clientPage.getTotalElements());
    }
}
