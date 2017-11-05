
require 'sinatra'
require 'sequel'

DB = Sequel.postgres(username: 'postgres')


get '/latest-vote' do
  latest_vote = DB[:votes].order(:date).last

  votes = DB[:vote_events].where(
    vote_id: latest_vote[:id]
  ).join(
    :people, id: :person_id
  ).to_a

  latest_vote.merge(votes: votes).to_json
end
