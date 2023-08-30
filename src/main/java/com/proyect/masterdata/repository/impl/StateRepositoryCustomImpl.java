package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.State;
import com.proyect.masterdata.repository.StateRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
@Repository
public class StateRepositoryCustomImpl implements StateRepositoryCustom {
    @PersistenceContext(name="entityManager")
    private EntityManager entityManager;

    @Override
    public Page<State> searchForState(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status){

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<State> criteriaQuery = criteriaBuilder.createQuery(State.class);
        Root<State> itemRoot = criteriaQuery.from(State.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name,user,status,criteriaBuilder,itemRoot);

        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> stateList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                stateList = listASC(sortColumn,criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                stateList = listDESC(sortColumn,criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(stateList);
        }else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<State> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        long count = getOrderCount(name,user,status);
        return new PageImpl<>(orderTypedQuery.getResultList(),pageable,count);
    }

    public List<Predicate> predicateConditions(
            String name,
            String user,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<State> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();

        if(name!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("name")),name.toUpperCase())));
        }

        if(user!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("name")),user.toUpperCase())));
        }

        if(status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if(!status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<State> itemRoot
    ){
        List<Order> stateList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("NAME")){
            stateList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }
        if(sortColumn.equalsIgnoreCase("USER")){
            stateList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }
        return stateList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<State> itemRoot
    ){
        List<Order> stateList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("NAME")){
            stateList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }
        if(sortColumn.equalsIgnoreCase("USER")){
            stateList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }
        return stateList;
    }

    private long getOrderCount(String name,String user,Boolean status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<State> itemRoot = criteriaQuery.from(State.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name,user,status,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
