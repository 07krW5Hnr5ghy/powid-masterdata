package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.repository.UserRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class UserRepositoryCustomImpl implements UserRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<User> searchForUser(
            String username,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<User> criteriaQuery = criteriaBuilder.createQuery(User.class);
        Root<User> itemRoot = criteriaQuery.from(User.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(username,status,criteriaBuilder,itemRoot);

        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){

        }
        return null;
    }

    public List<Predicate> predicateConditions(
            String username,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<User> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();
        if(username!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("user")),username.toUpperCase()
                            )
                    )
            );
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
            Root<User> itemRoot
    ){
        List<Order> userList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("USER")){
            userList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }
        return userList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<User> itemRoot
    ){
        List<Order> userList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("USER")){
            userList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }
        return userList;
    }

    private long getOrderCount(String username,Boolean status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<User> itemRoot = criteriaQuery.from(User.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(username,status,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
