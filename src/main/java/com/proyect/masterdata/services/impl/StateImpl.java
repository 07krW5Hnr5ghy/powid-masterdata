package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.State;
import com.proyect.masterdata.dto.StateDTO;
import com.proyect.masterdata.dto.request.RequestState;
import com.proyect.masterdata.dto.request.RequestStateSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.StateMapper;
import com.proyect.masterdata.repository.StateRepository;
import com.proyect.masterdata.services.IState;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class StateImpl implements IState {
    private final StateRepository stateRepository;
    private final StateMapper stateMapper;
    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions {
        try {
            stateRepository.save(stateMapper.stateToName(name.toUpperCase(),user.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<RequestStateSave> requestStateSaveList) throws BadRequestExceptions{
        try {
            stateRepository.saveAll(stateMapper.listRequestStateSaveToListState(requestStateSaveList)
                    .stream()
                    .map(
                            c -> {
                                State state = new State();
                                state.setName(c.getName().toUpperCase());
                                state.setStatus(c.getStatus());
                                state.setUser(c.getUser().toUpperCase());
                                return state;
                            }
                    ).collect(Collectors.toList())
            );
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public StateDTO update(RequestState requestState) throws BadRequestExceptions {
        try {
            requestState.setName(requestState.getName().toUpperCase());
            requestState.setUser(requestState.getUser().toUpperCase());
            State updatedState = stateMapper.requestStateToState(requestState);
            updatedState.setDateRegistration(new Date(System.currentTimeMillis()));
            State state = stateRepository.save(updatedState);
            return stateMapper.stateToStateDTO(state);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            stateRepository.deleteById(code);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions{
        try {
            stateRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<StateDTO> list() throws BadRequestExceptions{
        try {
            return stateMapper.listStateToListStateDTO(stateRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public StateDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return stateMapper.stateToStateDTO(stateRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public StateDTO findByName(String name) throws BadRequestExceptions{
        try {
            return stateMapper.stateToStateDTO(stateRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

}
