package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.State;
import com.proyect.masterdata.dto.StateDTO;
import com.proyect.masterdata.dto.response.ResponseState;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.StateMapper;
import com.proyect.masterdata.repository.StateRepository;
import com.proyect.masterdata.services.IState;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class StateImpl implements IState {
    private final StateRepository stateRepository;
    private final StateMapper stateMapper;
    @Override
    public List<StateDTO> listState() throws BadRequestExceptions {
        return stateMapper.stateListToStateListDTO(stateRepository.findAll());
    }

    @Override
    public ResponseState addState(String state) throws BadRequestExceptions {
        try{
            stateRepository.save(State.builder().name(state).status(true).build());
            return ResponseState.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseState deleteState(Long id) throws BadRequestExceptions {
        try{
            State record = stateRepository.findById(id).get();
            stateRepository.save(State.builder().name(record.getName()).dateRegistration(new Date(System.currentTimeMillis())).id(record.getId()).status(false).build());
            return ResponseState.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public StateDTO updateState(String name, Long id) throws BadRequestExceptions {
        return null;
    }


}
