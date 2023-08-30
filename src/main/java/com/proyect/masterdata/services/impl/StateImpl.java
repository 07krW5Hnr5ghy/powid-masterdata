package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.State;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.StateDTO;
import com.proyect.masterdata.dto.request.RequestState;
import com.proyect.masterdata.dto.request.RequestStateSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.StateMapper;
import com.proyect.masterdata.repository.StateRepository;
import com.proyect.masterdata.repository.StateRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IState;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class StateImpl implements IState {
    private final StateRepository stateRepository;
    private final StateMapper stateMapper;
    private final UserRepository userRepository;
    private final StateRepositoryCustom stateRepositoryCustom;
    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        State state;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            state = stateRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(state!=null){
            throw new BadRequestExceptions(Constants.ErrorStateExist.toUpperCase());
        }

        try {
            stateRepository.save(stateMapper.stateToName(RequestStateSave.builder()
                    .name(name.toUpperCase()).user(datauser.getUser().toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions,InternalErrorExceptions{
        User datauser;
        List<State> states;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            states = stateRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(!states.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorStateList.toUpperCase());
        }

        try {
            List<RequestStateSave> stateSaves = names.stream().map(data -> RequestStateSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
            stateRepository.saveAll(stateMapper.listStateToListName(stateSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public StateDTO update(RequestState requestState) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        State state;

        try{
            datauser = userRepository.findById(requestState.getUser().toUpperCase()).orElse(null);
            state = stateRepository.findById(requestState.getCode()).orElse(null);
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(state==null){
            throw new BadRequestExceptions(Constants.ErrorState.toUpperCase());
        }

        state.setName(requestState.getName().toUpperCase());
        state.setStatus(requestState.isStatus());
        state.setDateRegistration(new Date(System.currentTimeMillis()));
        state.setUser(datauser.getUser().toUpperCase());

        try {
            return stateMapper.stateToStateDTO(stateRepository.save(state));
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        State state;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            state = stateRepository.findById(code).orElse(null);
        }catch(RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(state==null){
            throw new BadRequestExceptions(Constants.ErrorState.toUpperCase());
        }

        try {
            state.setStatus(false);
            state.setDateRegistration(new Date(System.currentTimeMillis()));
            stateRepository.save(state);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<StateDTO> listState() throws BadRequestExceptions{
        List<State> states = new ArrayList<>();
        try{
            states = stateRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(states.isEmpty()){
            return Collections.emptyList();
        }
        return stateMapper.listStateToListStateDTO(states);
    }

    @Override
    public Page<StateDTO> list(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions{
        Page<State> statePage;
        try{
            statePage = stateRepositoryCustom.searchForState(name,user,sort,sortColumn,pageNumber,pageSize,true);
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(statePage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(stateMapper.listStateToListStateDTO(statePage.getContent()),
                statePage.getPageable(),statePage.getTotalElements());
    }

    @Override
    public Page<StateDTO> listStatusFalse(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions{
        Page<State> statePage;
        try{
            statePage = stateRepositoryCustom.searchForState(name,user,sort,sortColumn,pageNumber,pageSize,false);
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(statePage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(stateMapper.listStateToListStateDTO(statePage.getContent()),
                statePage.getPageable(),statePage.getTotalElements());
    }

    @Override
    public StateDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return stateMapper.stateToStateDTO(stateRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

}
