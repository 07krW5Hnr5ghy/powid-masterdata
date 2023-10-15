package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Connection;
import com.proyect.masterdata.dto.ConnectionDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ConnectionMapper;
import com.proyect.masterdata.repository.ConnectionRepository;
import com.proyect.masterdata.repository.ConnectionRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IConnection;
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
public class ConnectionImpl implements IConnection {
    private final UserRepository userRepository;
    private final ConnectionRepository connectionRepository;
    private final ConnectionRepositoryCustom connectionRepositoryCustom;
    private final ConnectionMapper connectionMapper;
    @Override
    public ResponseSuccess save(String url, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        boolean existsUrl;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            existsUrl = connectionRepository.existsByUrl(url);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(existsUrl){
            throw new BadRequestExceptions("Conexion ya existente");
        }
        try{
            connectionRepository.save(Connection.builder()
                            .url(url)
                            .status(true)
                            .dateRegistration(new Date(System.currentTimeMillis()))
                    .build());
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
    public ResponseSuccess saveAll(List<String> urls, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        List<Connection> connectionList;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            connectionList = connectionRepository.findByUrlIn(urls);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(!connectionList.isEmpty()){
            throw new BadRequestExceptions("Conexion ya existe");
        }
        try{
            connectionRepository.saveAll(urls.stream().map(url -> Connection.builder()
                    .url(url)
                    .status(true)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .build()).toList());
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
    public ResponseDelete delete(String url, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Connection connection;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            connection = connectionRepository.findByUrl(url);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(connection==null){
            throw new BadRequestExceptions("Conexion no existe");
        }
        try{
            connection.setStatus(false);
            connection.setDateRegistration(new Date(System.currentTimeMillis()));
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
    public Page<ConnectionDTO> list(String url, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<Connection> connectionPage;
        try{
            connectionPage = connectionRepositoryCustom.searchForConnection(url,sort,sortColumn,pageNumber,pageSize,true);
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(connectionPage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(connectionMapper.listConnectionToListConnectionDTO(connectionPage.getContent()),
                connectionPage.getPageable(),connectionPage.getTotalElements());
    }

    @Override
    public Page<ConnectionDTO> listStatusFalse(String url, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<Connection> connectionPage;
        try{
            connectionPage = connectionRepositoryCustom.searchForConnection(url,sort,sortColumn,pageNumber,pageSize,false);
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(connectionPage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(connectionMapper.listConnectionToListConnectionDTO(connectionPage.getContent()),
                connectionPage.getPageable(),connectionPage.getTotalElements());
    }
}
